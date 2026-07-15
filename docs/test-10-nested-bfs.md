# Nested Local Zip Scopes

This document illustrates how local zip scopes (`!zip_<name>`) behave when they are nested at different depths but share the same mapping hierarchy.

## Input YAML

In this example, we have two `!zip_a` tags. One is nested deeply inside `list2`, and the other is a sibling of `list2` (directly inside the object of `list1`).

```yaml
list1:
  - list2:
      - key21: !zip_a [1, 2]    # Nested zip
    key1: !zip_a [1, 2]         # Sibling zip (one level higher)
```

## Expected Output (Linked Zips)

Because both tags use the same local scope (`a`), they bubble up to the nearest common enclosing array (`list1`) and expand in lockstep. The array `list1` expands to exactly 2 elements, and the values are perfectly synchronized across the nested hierarchies:

```yaml
list1:
  - key1: 1
    list2:
      - key21: 1
  - key1: 2
    list2:
      - key21: 2
```

## Contrast: Unlinked Zips

If the user had used different scope names (e.g., `!zip_a` and `!zip_b`), the scopes would not be linked. 

```yaml
list1:
  - list2:
      - key21: !zip_b [1, 2]    # Scope 'b'
    key1: !zip_a [1, 2]         # Scope 'a'
```

In this case, `list2` would resolve its scope `b` locally first, and then `list1` would resolve its scope `a`. The result would look like this:

```yaml
list1:
  - key1: 1
    list2:
      - key21: 1
      - key21: 2
  - key1: 2
    list2:
      - key21: 1
      - key21: 2
```
