# OQ-72 Phase-2 vocabulary draft — AWAITING R2

Drafted 2026-07-04, after R1 ratification, before any assignment run.

## Drafting inputs and bias disclosure (the Phase-0 honesty note, applied)

- Inputs used: kernel_id, reading names, and the axiom-NAME population per kernel
  (`inventory.tsv` columns leg/kernel/reading/name). NOT used: tier, grounding (the diff's
  compared value), `cs_axiom_contradiction` facts, or any pairing structure.
- **Disclosure:** the drafter's session context contains the three named C1 control pairs
  (PROPOSAL.md / RECON.md §4) — unavoidable, since Phase 0 named them before Phase 2 by
  design. This is exactly why the pipeline is not sold as unbiasable end-to-end: the
  mitigation is (a) this draft proposes SLOTS, never per-axiom assignments; (b) R2
  ratification is the operator's seat, not a formality; (c) Phase-3 assignment is executed
  blind (fresh subagents, one reading + ratified vocabulary each, no session context).
- Concept atoms are kernel-scoped and namespaced `<kernel_id>__<slot>` — accidental
  cross-kernel equivalence is syntactically impossible.
- `no_slot` is always available at assignment: a vocabulary does NOT have to cover every
  axiom, and slot proliferation to force coverage is the over-fine failure mode (the C1
  rider's "granularity artifact"). Single-occupant subjects were mostly left OUT; an axiom
  with no slot reads blind, which is the honest state.

## Live leg (`testsets/`)

### digital_money_legitimacy (3 readings, 6 axioms)
| concept atom | definition |
|---|---|
| `digital_money_legitimacy__issuance_legitimacy_basis` | What confers legitimacy on digital money issuance — the contested ground (state authority, distributed consensus, reserve backing, regulatory permission or its unnecessity). |
| `digital_money_legitimacy__transaction_visibility` | Whether transaction visibility/surveillance is required for legitimate digital money operation or policy. |

R2 flag (closest call): `private_innovation_within_regulatory_perimeter` could read as an
issuance-legitimacy pole (private issuance legitimate inside the perimeter) or as no_slot;
the blind assigner rules on the ratified definitions.

### moral_causation_locus (3 readings, 7 axioms)
| concept atom | definition |
|---|---|
| `moral_causation_locus__causation_locus` | The primary causal seat of morally relevant behavior — stable character/disposition, situational structure, or their interaction (includes claims about character stability/instability across situations, which are this subject's poles). |
| `moral_causation_locus__accountability_intervention_locus` | Where moral accountability and corrective intervention properly attach — the individual or institutional/situational design. |

R2 flag (granularity seat, both defensible): `causation_locus` deliberately spans
"character is (un)stable across situations" AND "situation has primacy over disposition"
as poles of ONE subject. The finer alternative (separate `character_stability` and
`causal_primacy` slots) is drawable; ratify the granularity you rule correct.

### visual_evidentiary_authority (4 readings, 8 axioms)
| concept atom | definition |
|---|---|
| `visual_evidentiary_authority__verification_feasibility` | Whether the authenticity of images can still be verified at all/at scale (recoverable traces, detection gaps, scale impossibility). |
| `visual_evidentiary_authority__truth_warrant_source` | What warrants trust in an image as evidence — cryptographic/capture-time provenance, expert analysis, or social consensus. |
| `visual_evidentiary_authority__adaptation_response` | How evidentiary practice legitimately adapts to synthetic media (e.g. explicit acknowledgment regimes). |

## Haiku leg (`testsets_haiku/`)

### ai_governance_legitimacy (3 readings, 8 axioms)
| concept atom | definition |
|---|---|
| `ai_governance_legitimacy__governance_authority_source` | Who or what legitimately governs AI — magisterial/moral authority, market discipline, technocratic expertise. |
| `ai_governance_legitimacy__governing_value_priority` | The end AI development must serve and its priority ordering — human dignity, common good, aggregate welfare, efficiency. |
| `ai_governance_legitimacy__property_vs_collective_claims` | The standing of property rights versus solidarity/collective claims in constraining AI governance. |

### animal_moral_status (3 readings, 6 axioms)
| concept atom | definition |
|---|---|
| `animal_moral_status__moral_standing_basis` | Whether and on what basis animals hold moral standing — rights-bearing individuality, sentience-grounded obligation, or no independent standing. |
| `animal_moral_status__ownership_use_permissibility` | Whether ownership/use of animals is permissible — inherent violation, permissible under welfare constraint, or property-rights-protected. |

### marriage_authority_kernel (5 readings, 11 axioms)
| concept atom | definition |
|---|---|
| `marriage_authority_kernel__marital_authority_source` | Who legitimately governs marriage — ecclesiastical authority, interpretive tradition, communal self-determination/adjudication, the secular state, or individual liberty. |
| `marriage_authority_kernel__marriage_bond_nature` | What kind of thing the marriage bond is — indissoluble sacrament, community status, or individual contract. |
| `marriage_authority_kernel__norm_immutability` | Whether the norms governing marriage are divinely fixed/immutable or revisable. |

### software_source_status (4 readings, 8 axioms)
| concept atom | definition |
|---|---|
| `software_source_status__source_access_norm` | The normative status of source access/closure — inalienable right, categorical injustice of closure, or legitimate proprietary ownership. |
| `software_source_status__evaluation_standard` | The standard by which openness/closure is to be judged — categorical, instrumental, welfare-aggregating, context-dependent. |
| `software_source_status__empirical_effects` | Empirical claims about what openness/closure causes (quality, creator incentives). |

### tordesillas_demarcation_kernel (2 readings, 4 axioms)
| concept atom | definition |
|---|---|
| `tordesillas_demarcation_kernel__papal_authority_scope` | What papal authority can legitimately do in the territorial order — bind Christian powers by demarcation, license conquest. |
| `tordesillas_demarcation_kernel__acquisition_basis` | What legitimates territorial acquisition/monopoly — e.g. prior exploration. |
| `tordesillas_demarcation_kernel__indigenous_sovereignty_status` | Whether non-Christian peoples hold sovereign rights that constrain acquisition. |

### vatican_ii_doctrinal_authority (4 readings, 8 axioms)
| concept atom | definition |
|---|---|
| `vatican_ii_doctrinal_authority__doctrinal_continuity_status` | Whether conciliar doctrine stands in continuity, development, reversibility, or rupture with prior doctrine — including what doctrinal identity requires. |
| `vatican_ii_doctrinal_authority__interpretive_authority` | Who or what governs the council's ongoing interpretation — preserved magisterial authority or the council's spirit authorizing reform. |
| `vatican_ii_doctrinal_authority__event_composition` | Whether the council is a unified event or a composite of independent components with masked asymmetries. |

### wto_treaty_framework (2 readings, 6 axioms)
| concept atom | definition |
|---|---|
| `wto_treaty_framework__sdt_status` | The status of special & differential treatment — permanent structural accommodation or temporary exception. |
| `wto_treaty_framework__treaty_primary_purpose` | What the treaty framework fundamentally obligates — symmetric universal liberalization, non-discrimination, or development policy space as an equal-status right. |
| `wto_treaty_framework__tech_transfer_obligation` | Whether technology transfer is a compulsory obligation under the framework. |

## R2 ask

Ratify/edit each kernel's vocabulary (add/remove/merge/split slots, edit definitions).
The seed vocabulary is the operator's seat — author of record; rubber-stamping voids the
design. The two flagged granularity calls (digital_money_legitimacy perimeter axiom;
moral_causation_locus one-subject-vs-two) are the places most worth a deliberate ruling.
