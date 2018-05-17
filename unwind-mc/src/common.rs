use std::collections::{HashSet, VecDeque};
use std::hash::Hash;

pub enum Pick<T> {
    Return(Option<T>),
    Continue,
}

pub trait Graph<VId, V, E>
where
    VId: Copy + Eq + Hash,
    V: Copy,
{
    // Options/setup
    fn get_subgraph(&mut self, Option<HashSet<V>>) -> Self;
    fn with_edge_filter(&mut self, Fn(E) -> bool) -> Self;
    fn reverse_edges(&mut self) -> Self;

    // Getters
    fn contains(&self, &VId) -> bool;
    fn get_vertex(&self, &VId) -> &V;
    fn get_adjacent(&self, &VId) -> Vec<Result<(&VId, &E), String>>;

    // Implemented algorithms
    fn dfs_with(&self, start: &VId, consume: &mut FnMut(&V, &E) -> bool) -> bool {
        let mut stack = vec![(start, None)];
        let mut visited = HashSet::new();
        visited.insert(start);
        let mut visited_all_edges = false;
        while stack.len() > 0 {
            let (vid, edge) = stack.pop().unwrap();
            if consume(self.get_vertex(vid), edge.unwrap()) {
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

    fn dfs_pick<T>(&self, start: &VId, pick: &mut FnMut(&V, &E) -> Pick<T>) -> Option<T> {
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

    fn dfs(&self, start: &VId) -> Vec<V> {
        use common::Pick::*;

        // TODO: can be made lazy
        let mut result = Vec::<V>::new();
        self.dfs_pick::<()>(start, &mut |v, _e| {
            result.push(*v);
            Continue
        });
        result
    }

    fn bfs(&self, start: &VId) -> Vec<V> {
        let mut result = Vec::new();
        if !self.contains(start) {
            return result;
        }
        let mut queue = VecDeque::new();
        let mut visited = HashSet::new();
        queue.push_back(start);
        visited.insert(start);
        while queue.len() > 0 {
            let vid = queue.pop_front().unwrap();
            result.push(*self.get_vertex(vid));
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
        result
    }
}
