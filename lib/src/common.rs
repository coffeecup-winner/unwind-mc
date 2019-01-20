use std::collections::{HashSet, VecDeque};
use std::hash::Hash;

pub const REGISTER_SIZE: u32 = 4;

pub enum Pick<T> {
    Return(Option<T>),
    Continue,
}

pub trait Graph<VId, V, E>
where
    VId: Copy + Eq + Hash,
{
    // Options/setup
    fn set_subgraph(&mut self, Option<HashSet<VId>>) -> &mut Self;
    fn set_edge_filter(&mut self, Box<Fn(&E) -> bool>) -> &mut Self;
    fn reverse_edges(&mut self) -> &mut Self;

    // Getters
    fn contains(&self, &VId) -> bool;
    fn get_vertex(&self, &VId) -> &V;
    fn get_adjacent(&self, &VId) -> Vec<Result<(&VId, &E), String>>;

    // Implemented algorithms
    fn dfs_with(&self, start: &VId, consume: &mut FnMut(&V, Option<&E>) -> bool) -> bool {
        let mut stack = vec![(start, None)];
        let mut visited = HashSet::new();
        visited.insert(start);
        let mut visited_all_edges = false;
        while !stack.is_empty() {
            let (vid, edge) = stack.pop().unwrap();
            if consume(self.get_vertex(vid), edge) {
                for adj in self.get_adjacent(vid).iter().rev() {
                    match adj {
                        Err(_message) => {
                            visited_all_edges = false;
                        }
                        Ok((vertex, edge)) => {
                            if !visited.contains(vertex) {
                                stack.push((vertex, Some(edge)));
                                visited.insert(vertex);
                            }
                        }
                    }
                }
            }
        }
        visited_all_edges
    }

    fn dfs_pick<T>(&self, start: &VId, pick: &mut FnMut(&V, Option<&E>) -> Pick<T>) -> Option<T> {
        use common::Pick::*;

        let mut result = None;

        self.dfs_with(start, &mut |v, e| match pick(v, e) {
            Return(r) => {
                result = r;
                false
            }
            Continue => true,
        });
        result
    }

    fn bfs_with(&self, start: &VId, consume: &mut FnMut(&V) -> bool) -> () {
        if !self.contains(start) {
            return ();
        }
        let mut queue = VecDeque::new();
        let mut visited = HashSet::new();
        queue.push_back(start);
        visited.insert(start);
        while !queue.is_empty() {
            let vid = queue.pop_front().unwrap();
            consume(self.get_vertex(vid));
            for adj in self.get_adjacent(vid).iter() {
                match adj {
                    Err(_message) => { /* TODO: log */ }
                    Ok((v, _e)) => {
                        if visited.insert(v) {
                            queue.push_back(v);
                        }
                    }
                }
            }
        }
        ()
    }
}
