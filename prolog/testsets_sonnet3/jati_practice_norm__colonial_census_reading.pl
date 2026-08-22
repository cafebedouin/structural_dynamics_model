% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__colonial_census_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__colonial_census_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jati_practice_norm__colonial_census_reading
 *   human_readable: Colonial Census Reification of Jati Categories
 *   domain: social anthropology/religious studies/political economy
 *
 * SUMMARY:
 *   This story instantiates the colonial_census_reading of the
 *   jati_practice_norm kernel: the claim that jati categories were
 *   substantially stabilized and reified by the external administrative
 *   apparatus of the colonial census (and its ethnographic survey machinery)
 *   for the purpose of governance legibility, not because those categories
 *   reflected a stable pre-existing order. The census did not merely record
 *   caste; it froze a moving target into a fixed, hierarchically ranked,
 *   countable table, and that table then became the administrative substrate
 *   for personal law, recruitment, and — after independence — reservation
 *   policy. Extraction here runs through legibility: colonial administration
 *   and its bureaucratic apparatus gained a governable population at the cost
 *   of local communities' capacity to keep renegotiating their own
 *   boundaries, and of the accuracy of the categories themselves. This is a
 *   distinct constraint from the orthodox_textual_reading (which locates the
 *   source of jati boundaries in scriptural varna doctrine and treats
 *   deviation as ritual pollution — a claim about textual authority, not
 *   administrative construction) and from the localized_practice_reading
 *   (which holds that jati boundaries were, and largely remained outside the
 *   census's reach, fluid coordination norms under continuous local
 *   renegotiation — a claim about the persistence of local agency, not
 *   administrative capture). The three readings are not the same constraint
 *   measured differently; they make different claims about what fixes jati
 *   boundaries and to whose benefit, so each is authored as its own story
 *   with its own epsilon.
 *
 * KEY AGENTS:
 *   - colonial_administration: agenda-setting institutional actor designing the census taxonomy (institutional/analytical)
 *   - census_enumerating_bureaucracy: operational arm forcing local ambiguity into fixed categories (institutional/analytical)
 *   - upwardly_mobile_caste_associations: organized actors who both benefited from and were locked by the fixed table (organized/constrained)
 *   - boundary_ambiguous_communities, occupationally_mobile_lineages, locally_fluid_jati_clusters: powerless groups whose lived fluidity was erased by the fixed record (powerless/trapped)
 *   - post_independence_state_apparatus: inherited and entrenched the frozen taxonomy for a new governance purpose (institutional/analytical)
 *   - anthropologists_and_historians: analytical observers documenting the pre/post-census gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, 0.58).
domain_priors:suppression_score(jati_practice_norm__colonial_census_reading, 0.62).
domain_priors:theater_ratio(jati_practice_norm__colonial_census_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__colonial_census_reading, tangled_rope).
narrative_ontology:human_readable(jati_practice_norm__colonial_census_reading, "Colonial Census Reification of Jati Categories").
narrative_ontology:topic_domain(jati_practice_norm__colonial_census_reading, "social anthropology/religious studies/political economy").

domain_priors:requires_active_enforcement(jati_practice_norm__colonial_census_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__colonial_census_reading, '1716c03d-b6db-42e1-bf1f-a9adb94d54c4').
narrative_ontology:cs_kernel_codification('1716c03d-b6db-42e1-bf1f-a9adb94d54c4', formalized).
narrative_ontology:cs_authority_grounding('1716c03d-b6db-42e1-bf1f-a9adb94d54c4', extraction).
narrative_ontology:cs_interpretation_layer_present('1716c03d-b6db-42e1-bf1f-a9adb94d54c4').
narrative_ontology:cs_reading_relation('1716c03d-b6db-42e1-bf1f-a9adb94d54c4', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('1716c03d-b6db-42e1-bf1f-a9adb94d54c4', jati_practice_norm__localized_practice_reading, influences).
narrative_ontology:cs_axiom('1716c03d-b6db-42e1-bf1f-a9adb94d54c4', foundational, administrative_legibility_as_boundary_source).
narrative_ontology:cs_axiom_status(administrative_legibility_as_boundary_source, holdable).
narrative_ontology:cs_axiom_grounding('1716c03d-b6db-42e1-bf1f-a9adb94d54c4', administrative_legibility_as_boundary_source, empirically_contingent).
narrative_ontology:cs_axiom('1716c03d-b6db-42e1-bf1f-a9adb94d54c4', secondary, external_enumeration_overrides_local_self_definition).
narrative_ontology:cs_axiom_status(external_enumeration_overrides_local_self_definition, holdable).
narrative_ontology:cs_axiom_grounding('1716c03d-b6db-42e1-bf1f-a9adb94d54c4', external_enumeration_overrides_local_self_definition, conventional).
narrative_ontology:cs_reference_frame('1716c03d-b6db-42e1-bf1f-a9adb94d54c4', pre_census_localized_negotiation).
narrative_ontology:cs_drift_state('1716c03d-b6db-42e1-bf1f-a9adb94d54c4', post_independence_reservation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1716c03d-b6db-42e1-bf1f-a9adb94d54c4', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__colonial_census_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, colonial_administration).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, census_enumerating_bureaucracy).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, upwardly_mobile_caste_associations).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, boundary_ambiguous_communities).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, occupationally_mobile_lineages).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, locally_fluid_jati_clusters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, post_independence_state_apparatus).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, upwardly_mobile_caste_associations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and runs the decennial census apparatus, commissioning ethnographic surveys (e.g. Risley-style anthropometric classification) to sort the population into a fixed, hierarchically ranked table of jatis for the purposes of taxation, recruitment, and administration of personal law. Requires legible, countable categories and has no incentive to represent the fluidity local communities actually practice; benefits from a stabilized taxonomy that makes governance tractable regardless of its fidelity to lived practice.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, colonial_administration, agenda_setter,
    institutional, generational, analytical, national).

% Local enumerators and district officers must resolve ambiguous self-reported identities into a single fixed slot on the census schedule. They benefit from having a settled reference table to work from and from the prestige/promotion structures built around administering it; they have no stake in preserving local nuance and every incentive to force convergence.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, census_enumerating_bureaucracy, beneficiary,
    institutional, biographical, analytical, national).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, census_enumerating_bureaucracy, agenda_setter).

% Caste associations (sabhas) that organized in the census era used the fixed, published rank-tables strategically, petitioning enumerators for reclassification to a higher-ranked varna-adjacent label. They gained real mobility leverage from the stabilized taxonomy but were simultaneously locked by it — once the census fixed a rank, the same rigidity that let them petition upward also foreclosed the older, more fluid renegotiation channels they had used before enumeration existed.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, upwardly_mobile_caste_associations, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, upwardly_mobile_caste_associations, payer).

% Communities whose practiced identity spanned or straddled what the census insisted were discrete categories were forced into a single box, often one assigned by an outside enumerator's judgment rather than the community's own account of itself. The fixed category becomes the legal and administrative reality (personal law, reservation eligibility, land records) regardless of what the community actually understood itself to be; there is no route back to the pre-census ambiguity once the record is set.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, boundary_ambiguous_communities, payer,
    powerless, generational, trapped, local).

% Lineages that had historically moved between occupational/ritual designations as circumstances changed (a common pre-colonial pattern) find that the census apparatus converts a snapshot of their occupation into a permanent hereditary label, cutting off the informal channels by which status had previously been renegotiated across generations.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, occupationally_mobile_lineages, payer,
    powerless, biographical, trapped, local).

% Clusters of sub-groups that had negotiated boundaries locally and variably by region, marriage circle, and occasion are merged or split according to the census's need for a manageable master list, erasing distinctions that mattered locally and imposing distinctions that did not previously exist, with no mechanism to contest the resulting record.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, locally_fluid_jati_clusters, payer,
    powerless, generational, trapped, local).

% Inherited the colonial-era category tables wholesale as the administrative basis for affirmative action (reservation) policy, entrenching the frozen categories for a new governance purpose. Benefits from an existing legible taxonomy it did not have to build, but this also transmits the colonial artifact's rigidities into a system meant to remedy historical disadvantage.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, post_independence_state_apparatus, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, post_independence_state_apparatus, observer).

% Document the gap between the pre-census ethnographic record (showing fluid, contested, regionally-varying jati boundaries) and the post-census administrative record (showing a fixed hierarchical table), providing the evidentiary basis for the claim that the census apparatus reified rather than described caste structure.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, anthropologists_and_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__colonial_census_reading, diffuse).
narrative_ontology:fixing_cost_class(jati_practice_norm__colonial_census_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides colonial (and later national) administration with a legible, countable, comparable population taxonomy sufficient for taxation, recruitment, personal-law adjudication, and later reservation policy — solving a genuine governance problem of needing stable categories to administer a large and otherwise irreducibly heterogeneous population.
% TRANSFER_FUNCTION: Moves classificatory authority away from local communities' own continuous, context-dependent renegotiation of jati boundaries and concentrates it in an external administrative record; moves real material stakes (legal status, reservation eligibility, marriage-market standing) onto whatever label the enumerator recorded, transferring bargaining power to communities positioned to petition the fixed table (upwardly mobile associations) at the expense of communities whose identity did not map onto the table's categories.
% ABSENT_VOICES: The communities whose lived practice spanned or blurred the census categories were not consulted on the taxonomy's design; the ethnographic surveys underlying the rank-tables (e.g. caste handbooks, anthropometric surveys) were authored by colonial officials and cooperating elite informants, not by the boundary-ambiguous or occupationally mobile groups who bore the classificatory cost.
% DISAPPEARANCE_RATIONALE: If the census-derived category table vanished, colonial-era governance legibility would have been genuinely disrupted (world_rearranges from the administrator's seat) — but post-independence reservation policy has since built durable legal and political structures directly on top of the inherited categories, so a present-day disappearance is contested: some parties (those advantaged by current reservation status) would experience real material rearrangement, while others argue the categories were never accurate to begin with and their removal would only restore a pre-existing local fluidity (world_unchanged from that seat).
% FOUNDING_PROBLEM: Colonial administration needed a governable, comparable population taxonomy to run taxation, army recruitment, and personal-law courts across an enormous and, from the administrator's standpoint, illegibly heterogeneous set of local practices.
% FOUNDING_PROBLEM_CORROBORATION: Historians of colonial administration (e.g. work tracing the Risley census methodology and its critique) attest, from outside both the colonial administration and the caste associations that benefited from reclassification, that the original administrative-legibility problem no longer exists in its colonial form, yet the frozen category table persists as the substrate of post-independence reservation policy — a genealogy corroborated by anthropological fieldwork documenting pre-census fluidity that the fixed record erased.
narrative_ontology:disappearance_verdict(jati_practice_norm__colonial_census_reading, contested).
narrative_ontology:founding_problem_status(jati_practice_norm__colonial_census_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__colonial_census_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jati_practice_norm__colonial_census_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__colonial_census_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__colonial_census_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__colonial_census_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58 by interval end) sits at moderate-tangled-rope level, not snare level, because the census apparatus did solve a genuine coordination problem (governing a huge, otherwise administratively illegible population) — the coordination function is real, which is what distinguishes this from pure extraction. But the same structure imposed asymmetric costs on communities whose identity did not map cleanly onto the resulting table, and those costs compounded as the frozen categories were inherited by post-independence institutions for purposes (reservation policy) the colonial census was never designed for — hence extraction rises modestly over the measured interval even as the original administrative rationale (founding_problem_status: dead) disappeared. Suppression starts high (0.7) during the active colonial enumeration and enforcement period, dips somewhat post-independence as direct coercive enumeration eased, then partially re-hardens (0.62 by T=100) as reservation-policy litigation and eligibility disputes re-invoke the fixed categories as binding legal fact. Theater ratio rises across the interval (0.15 to 0.40) as the administrative-legibility justification becomes increasingly performative — the original governance need is gone, but the category table is defended using legibility-era language even though its live function is now allocating reservation benefits and organizing electoral/political blocs.
 *
 * DIRECTIONALITY LOGIC:
 *   Colonial administration and its bureaucracy sit at the beneficiary end: they collect governability, not material rents, but that collection is the constraint's coordination payoff and it costs them nothing to maintain once built. Upwardly mobile caste associations are dual-positioned: they used the fixed table instrumentally (beneficiary) but were also locked into a rigid structure that removed the older renegotiation channels they had previously relied on (payer) — this is exactly the kind of asymmetric secondary effect that justifies a dual role rather than forcing a single directionality. Boundary-ambiguous, occupationally mobile, and locally fluid groups are unambiguous targets: trapped exit, powerless, and bearing the entire cost of a classification exercise they did not design and cannot revise.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (colonial administrative legibility) is dead — no one administers British Indian taxation or recruitment any longer — yet the arrangement (the fixed jati table) persists because it was captured by a successor institution (the post-independence state) for a different purpose (reservation policy) that itself now depends on the same frozen categories remaining stable. This is precisely the mismatch the six-questions R5 interview is built to catch: founding_problem_status=dead paired with a disappearance_verdict of contested rather than world_unchanged signals that the arrangement has been repurposed rather than genuinely obsolete, which should read as a capture/zombie flag rather than either a clean mountain (it was never natural) or a clean piton (there IS a concentrated beneficiary class in the successor institution and in the caste associations who gained standing under the fixed table).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reification_vs_description_ambiguity,
    'Did the colonial census apparatus actually CREATE fixed jati boundaries that did not previously exist in that form, or did it merely record and formalize boundary tendencies that were already hardening for independent reasons (e.g. endogamous consolidation, regional political economy)?',
    'Comparative ethnographic and archival analysis of pre-census (17th-18th century) versus census-era (late 19th century) local records for the same regions, tracing whether boundary rigidity predates or postdates enumeration.',
    'If the census substantially created the rigidity, this reading''s extraction claim is strong and the tangled_rope classification with heavy suppression is well-founded. If the census mainly recorded pre-existing hardening, the extraction attributable to the ADMINISTRATIVE apparatus specifically (as opposed to broader socioeconomic forces) is smaller than authored here, and this constraint''s epsilon should be revised downward relative to a broader socioeconomic-hardening constraint not modeled in this story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reification_vs_description_ambiguity, empirical, 'Whether colonial enumeration caused or merely recorded jati boundary rigidification.').

omega_variable(
    kernel_reading_location_of_disagreement,
    'Where exactly do the three sibling readings of the jati_practice_norm kernel disagree — is it about WHAT jati boundaries fundamentally are (textual doctrine vs. administrative artifact vs. living local negotiation), or about WHICH historical period''s boundary structure is authoritative for present-day classification?',
    'A structural mapping exercise comparing the orthodox_textual_reading''s grounding in scriptural varna texts, this reading''s grounding in colonial administrative records, and the localized_practice_reading''s grounding in ongoing ethnographic fieldwork, to determine whether these are genuinely rival ontological claims (mutually exclusive) or complementary partial descriptions applicable to different regions/periods.',
    'If the three readings are genuinely mutually exclusive (each denies the others'' core mechanism), the kernel exhibits a forecloses relationship somewhere in the triplet rather than the coexists_with/influences pattern authored here. If they are complementary (different regions/periods), all three can legitimately coexist as this story assumes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Structural location of disagreement among the three kernel readings.').

omega_variable(
    reservation_policy_dependency,
    'Would dismantling the colonial-derived category table cause India''s post-independence reservation (affirmative action) system to collapse, or could an equivalent legibility be reconstructed on different (e.g. self-identification or socioeconomic) grounds without material loss to intended beneficiaries?',
    'Comparative policy analysis of jurisdictions that have transitioned affirmative-action eligibility criteria away from colonial-era census categories, tracking whether beneficiary populations and program integrity were preserved.',
    'If reservation policy is structurally dependent on the frozen categories, the disappearance_verdict of ''contested'' understates the current stakes and the constraint''s persistence is better read as load-bearing infrastructure rather than pure inertia. If reservation policy could be reconstructed on other grounds, the persistence is closer to pure institutional path-dependency (piton-adjacent) riding on top of what is authored here as tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reservation_policy_dependency, empirical, 'Whether current reservation policy is load-bearing on the colonial category table or merely inherited it as convenient path dependency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__colonial_census_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__colonial_census_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jati_tr_t20, jati_practice_norm__colonial_census_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(jati_tr_t40, jati_practice_norm__colonial_census_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(jati_tr_t60, jati_practice_norm__colonial_census_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(jati_tr_t80, jati_practice_norm__colonial_census_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement(jati_tr_t100, jati_practice_norm__colonial_census_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__colonial_census_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(jati_be_t20, jati_practice_norm__colonial_census_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(jati_be_t40, jati_practice_norm__colonial_census_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(jati_be_t60, jati_practice_norm__colonial_census_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(jati_be_t80, jati_practice_norm__colonial_census_reading, base_extractiveness, 80, 0.57).
narrative_ontology:measurement(jati_be_t100, jati_practice_norm__colonial_census_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t0, jati_practice_norm__colonial_census_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(jati_su_t20, jati_practice_norm__colonial_census_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(jati_su_t40, jati_practice_norm__colonial_census_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(jati_su_t60, jati_practice_norm__colonial_census_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(jati_su_t80, jati_practice_norm__colonial_census_reading, suppression_requirement, 80, 0.63).
narrative_ontology:measurement(jati_su_t100, jati_practice_norm__colonial_census_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__colonial_census_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jati_practice_norm__colonial_census_reading, 0.1).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__localized_practice_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three members of the jati_practice_norm kernel family. orthodox_textual_reading grounds jati in fixed scriptural varna doctrine (a distinct authority_grounding and a much higher accessibility_collapse, closer to mountain/tangled_rope framed as sacred order). localized_practice_reading denies substantial fixation occurred at all and models jati as an ongoing rope-like coordination norm with low suppression and high local renegotiation capacity. This story (colonial_census_reading) occupies the middle: moderate extraction, tangled_rope, driven specifically by external administrative reification rather than either textual authority or continuous local practice. All three share the same underlying kernel object (jati boundaries) but author structurally distinct epsilon values and are linked here rather than merged, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
