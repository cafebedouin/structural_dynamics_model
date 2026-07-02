# OQ-136 hand-read — extraction_unnameable (3) + manufactured_consensus_candidate (9)

Protocol per PROPOSAL.md: per member, the in-file authored fields; is the unnamed/excluded party
genuinely absent from the situation, or an authoring oversight? Footing: **RULED** = in-file
witness quoted; **INFERRED** = cross-sibling/analogical only. Line numbers are per the files at
git `0ba48b4c` (execution rev, see RECON.md).

## extraction_unnameable (n=3, all claude-haiku-4-5, all also in no_agent_seats)

The bucket fires on a CONJUNCTION (extractive type ∧ no authored victim ∧ no beneficiary-side
agent seat). The hand-read splits the two absence limbs — they have opposite verdicts:

### animal_status_kernel__property_reading
- Authored: `constraint_beneficiary(_, property_owners)` + `(_, economic_beneficiaries_of_animal_use)`
  (:143–144); **0 `constraint_stakeholder/7` facts**; no `constraint_victim/2`.
- Seat limb — **ARTIFACT (RULED)**: the file's own commentary plans the seats it never authors:
  ":195 *The property-owner seat and the analytical observer seat should compute radically
  differently… Owners are beneficiaries with high power and arbitrage-exit*". The extractor is
  named at the constraint_beneficiary level and described as a seat in prose; the stakeholder
  fact layer is simply empty.
- Victim limb — **GENUINE-to-the-reading (RULED)**: ":195 *The constraint-as-written excludes
  animals from the victim-set per the property reading's logic: the reading does not recognize
  animals as moral patients, so it does not recognize them as victims. This exclusion is the
  reading's structural signature*". The missing victim is the reading's deliberate shape.

### jewish_self_determination__indigenous_return_reading
- Authored: `constraint_beneficiary(_, jewish_claimants_to_ancestral_land)` (:153); **0
  stakeholder facts**; no victim fact.
- Seat limb — **ARTIFACT (RULED)**: prose enumerates the full seat structure that was never
  emitted as facts: ":97 *palestinian_presence_interpreters (organized, constrained)*"; ":204
  *The beneficiary seat (jewish_claimants) and the payer seat (palestinian_presence_interpreters)
  should compute radically different classifications*"; ":207 gives the payer seats d values
  (0.6–0.8). A story that assigns d ranges to payer seats intended to author them.
- Victim limb — **GENUINE-to-the-reading (RULED)**: ":207 *Under this reading's own terms,
  Palestinians are not victims of extraction but of historical population movement or legitimate
  subordination*".

### secession_legitimacy_boundary__constitutional_impossibility_reading
- Authored: `constraint_beneficiary(_, federal_authority_structure)` (:142); **0 stakeholder
  facts**; no victim fact.
- Seat limb — **ARTIFACT (RULED)**: ":91 *separatist_provincial_movement: Payer; constrained by
  the rule; identity-locked*"; ":193 *The engine will compute per-seat classifications from the
  stakeholder positions*" — the prose plans payer/beneficiary seats; none are authored.
- Victim limb — **GENUINE-to-the-reading (INFERRED)**: the story's own core open question
  (:228–230) is whether the suppression is "procedural enforcement cost" (mountain) or "active
  coercion of a legitimacy claim" (snare); authoring a victim fact would pre-judge it. The prose
  frames separatists as *payer*, not victim. No sentence rules the victim question explicitly,
  so this stays INFERRED.

**Bucket verdict**: membership is artifact-inflated. The extractor-seat limb is a generation
gap (haiku path emits beneficiary/prose but no stakeholders[]); fixing it would migrate all
three to `extraction_fired` (nameable blindspot). The victim limb is the readings' genuine
structure in 2 of 3 (RULED) and plausibly 3 of 3.

## manufactured_consensus_candidate (n=9, model-mixed — consistent with no clustering)

Every member authors real seats (4–8 `constraint_stakeholder/7` facts) with a deliberate
`excluded` role; the question is whether the excluded party is substantively in the situation.

| member | excluded seat(s) | in-file witness | verdict |
|---|---|---|---|
| basic_law_interpretive_boundary__parliamentary_sovereignty_reading | international_observers_and_treaty_partners | :99 "*Excluded from Knesset process; would contest that sovereignty is limited by treaty obligations*" | **genuine (RULED)** |
| conceptual_framework_reading | implementation_seekers | :197 "*Implementation-seekers are excluded by the reading's own frame: their need for organizational grounding is what this reading treats as category error*" | **genuine (RULED)** |
| demographic_resource_allocation | migrant_worker_households | :190 ABSENT_VOICES "*structurally excluded from the planning grid due to hukou restrictions. They would argue for portable benefits*" | **genuine (RULED)** |
| demographic_skill_mismatch_c0 | younger_potential_entrants | :172 ABSENT_VOICES "*structurally present but culturally excluded — their voice… is drowned out*" | **genuine (RULED)** — cultural not structural exclusion, but authored deliberately and argued |
| fictional_construct_reading | engineering_practitioners | :87 "*structurally excluded because the document makes no implementation claims in their domain*" | **genuine (RULED)** |
| jewish_sovereignty_palestine__cultural_zionist_reading | liberal_nationalist_zionists | :203 "*excluded from this reading's core premise, as they prioritize statehood*"; the excluded party IS the sibling kernel reading (`cs_reading_relation … liberal_nationalist_reading, coexists_with`, :113) | **genuine (RULED)** — cross-sibling structure confirms: the excluded seat is occupied by a sibling reading of the same kernel |
| neutron_star_bombardment_reading | superheavy_element_researchers | :98 "*excluded from this reading's framework; advocate alternative mechanism*" | **genuine (RULED)** — scientific-contest flavor: exclusion is framework-level |
| radiative_levitation_stratification | alternative_mechanism_proponents | :182 ABSENT_VOICES "***No voices are structurally absent** — … Alternative mechanism proponents participate fully in the discourse; their exclusion is evidential, not structural*" | **AUTHORING INCONSISTENCY (RULED)** — the fact layer authors `excluded` while the prose disavows structural exclusion; per the file's own text this member is a FALSE POSITIVE of the mcc flag (the authored role vocabulary is coarser than the prose's structural/evidential distinction) |
| refugee_convention_text__expansive_humanitarian_reading | restrictive_sovereigntist_governments, procedural_integrity_advocates | :214 ABSENT_VOICES "*structurally excluded from the humanitarian frame; they would argue for narrow definitions*" | **genuine (RULED)** |

**Bucket verdict**: 8/9 genuine — the flag is the mechanism working as designed on deliberately
authored exclusions; the unanimity-with-named-absentee shape is real corpus structure, not a
generation artifact (consistent with the null clustering result). 1/9 (radiative_levitation) is
a per-member authoring inconsistency, and it names a latent vocabulary gap: the `excluded` role
atom cannot express "evidential, not structural" exclusion.
