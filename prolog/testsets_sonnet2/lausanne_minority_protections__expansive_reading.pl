% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__expansive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__expansive_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: lausanne_minority_protections__expansive_reading
 *   human_readable: Lausanne Minority Protections — Expansive (Functional Continuity) Reading
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This constraint instantiates the expansive reading of the Lausanne
 *   minority-protections kernel: that the treaty guarantees not merely
 *   individual freedom of worship but functional continuity of the pre-1923
 *   institutional architecture of religious governance — self-administered
 *   courts and foundations, clergy formation via theological schools, and
 *   community control of religious property. Under this reading, the closure
 *   of the Halki Seminary in 1971 and subsequent foundation-property disputes
 *   are treaty violations, not internal administrative matters. This is a
 *   genuine coordination rope from the reading's own perspective: it solves
 *   the real problem of how a modernizing nation-state absorbs pre-existing
 *   minority self-governance without destroying it, and its beneficiaries
 *   (the communities) are not extracting rent from anyone — they simply
 *   retain what the reading holds they were promised. The theater_ratio rises
 *   over the interval (0.10 to 0.30) reflecting increasing diplomatic and
 *   legal performance around the claim (statements, court filings, EU
 *   accession-process rhetoric) relative to actual functional restoration
 *   (Halki remains closed as of 2024).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__expansive_reading, 0.28).
domain_priors:suppression_score(lausanne_minority_protections__expansive_reading, 0.42).
domain_priors:theater_ratio(lausanne_minority_protections__expansive_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__expansive_reading, rope).
narrative_ontology:human_readable(lausanne_minority_protections__expansive_reading, "Lausanne Minority Protections — Expansive (Functional Continuity) Reading").
narrative_ontology:topic_domain(lausanne_minority_protections__expansive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__expansive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__expansive_reading, '637ab2a2-48ed-4b4d-91aa-28b2b0d5a526').
narrative_ontology:cs_kernel_codification('637ab2a2-48ed-4b4d-91aa-28b2b0d5a526', fixed_text).
narrative_ontology:cs_authority_grounding('637ab2a2-48ed-4b4d-91aa-28b2b0d5a526', lineage).
narrative_ontology:cs_interpretation_layer_present('637ab2a2-48ed-4b4d-91aa-28b2b0d5a526').
narrative_ontology:cs_reading_relation('637ab2a2-48ed-4b4d-91aa-28b2b0d5a526', lausanne_minority_protections__restrictive_reading, forecloses).
narrative_ontology:cs_reading_relation('637ab2a2-48ed-4b4d-91aa-28b2b0d5a526', lausanne_minority_protections__guarantor_reading, coexists_with).
narrative_ontology:cs_axiom('637ab2a2-48ed-4b4d-91aa-28b2b0d5a526', foundational, institutional_continuity_is_the_protected_object).
narrative_ontology:cs_axiom_status(institutional_continuity_is_the_protected_object, holdable).
narrative_ontology:cs_axiom_grounding('637ab2a2-48ed-4b4d-91aa-28b2b0d5a526', institutional_continuity_is_the_protected_object, conventional).
narrative_ontology:cs_axiom('637ab2a2-48ed-4b4d-91aa-28b2b0d5a526', secondary, clergy_formation_is_a_treaty_guaranteed_function).
narrative_ontology:cs_axiom_status(clergy_formation_is_a_treaty_guaranteed_function, holdable).
narrative_ontology:cs_axiom_grounding('637ab2a2-48ed-4b4d-91aa-28b2b0d5a526', clergy_formation_is_a_treaty_guaranteed_function, conventional).
narrative_ontology:cs_reference_frame('637ab2a2-48ed-4b4d-91aa-28b2b0d5a526', ottoman_millet_derived_self_governance).
narrative_ontology:cs_drift_state('637ab2a2-48ed-4b4d-91aa-28b2b0d5a526', post_halki_closure_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('637ab2a2-48ed-4b4d-91aa-28b2b0d5a526', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__expansive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, greek_orthodox_patriarchate).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, armenian_apostolic_community).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, jewish_community_of_istanbul).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, minority_theological_schools).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__expansive_reading, functional_continuity_doctrine).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__expansive_reading, institutional_self_administration_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers its own clergy appointments, internal courts, and charitable foundations under the claim that Lausanne guarantees continuity of pre-1923 arrangements. Depends entirely on the expansive reading holding for its theological school (Halki, closed since 1971) to reopen and for property held by its foundations to remain under its own administration rather than state trusteeship. Cannot relocate its seat; its institutional identity is fused with continuous presence in Istanbul.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, greek_orthodox_patriarchate, beneficiary,
    moderate, civilizational, identity_locked, national).

% Runs parish schools, church courts for personal status matters, and community foundations under the same functional-continuity claim. Members can in principle emigrate, but the community's institutions cannot: their survival as self-administering bodies depends on the treaty being read to protect governance structures, not merely individual worship.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, armenian_apostolic_community, beneficiary,
    moderate, generational, constrained, national).

% Maintains a chief rabbinate, community foundations, and property holdings under the expansive reading's protection. Smaller and more mobile than the other communities, but its remaining institutions still depend on the same interpretive outcome to retain self-administration rather than being folded into general civil administration.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, jewish_community_of_istanbul, beneficiary,
    moderate, generational, constrained, national).

% Institutions (seminaries, clergy-training schools) whose legal right to operate and issue recognized credentials exists only if the expansive reading of Lausanne prevails; under the restrictive reading they are ordinary private schools subject to secular curriculum mandates that would end clergy formation.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, minority_theological_schools, beneficiary,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(lausanne_minority_protections__expansive_reading, minority_theological_schools).

% Administers the domestic legal regime that determines, in practice, whether minority foundations retain self-governance, control their own property, and may train clergy. Can choose which reading of Lausanne to apply through statute, foundation law amendments, and administrative rulings, and bears the diplomatic cost of departing from the expansive reading when guarantor states object.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, turkish_state, agenda_setter,
    institutional, civilizational, arbitrage, national).

% France, UK, and other Lausanne signatories periodically raise minority treatment in diplomatic contexts. They can lend weight to the expansive reading through statements and pressure but do not administer the institutions themselves and have limited capacity to compel outcomes.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, guarantor_states, observer,
    institutional, generational, analytical, continental).

% Some domestic political and legal voices view minority institutional self-administration as inconsistent with equal citizenship under a secular constitutional order and are not treated as parties to the treaty-interpretation question, though they would object to institutional carve-outs if consulted.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, domestic_secularist_constituencies, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a durable framework letting religious minority communities administer their own internal governance — clergy appointment, community courts for personal status, control of communal property and charitable foundations, and training of religious leadership — without those functions being absorbed into ordinary state administration, in exchange for the communities not seeking separate political status.
% TRANSFER_FUNCTION: Moves administrative authority over minority-community religious, educational, and property matters from the general state apparatus to community-internal bodies, and moves diplomatic attention/legitimacy from unilateral domestic discretion to a treaty-anchored claim the communities and guarantor states can invoke.
% ABSENT_VOICES: Domestic secularist constituencies who view institutional carve-outs as inconsistent with equal citizenship are not parties to the interpretive contest; individual minority-community members who might prefer weaker community-court jurisdiction over their personal status matters are also not separately consulted — the community institutions speak for them.
% DISAPPEARANCE_RATIONALE: If the expansive reading lost the interpretive contest entirely, the Ecumenical Patriarchate's foundations could pass to state trusteeship, theological schools could not reopen or would operate as ordinary private institutions barred from clergy formation, and community courts for personal status would dissolve into general civil jurisdiction — a structural collapse of institutional self-administration for these communities, not a cosmetic change.
% FOUNDING_PROBLEM: At the 1923 Lausanne negotiations, departing Ottoman minority arrangements needed a successor framework so newly-formed Turkey would not simply dissolve centuries-old community self-governance structures (millet-derived courts, church/synagogue property, clergy training) as the state modernized and centralized.
% FOUNDING_PROBLEM_CORROBORATION: The Ecumenical Patriarchate and academic specialists in minority law (writing independently of the communities) attest the founding problem remains live because functional erosion (Halki's closure, foundation-property disputes) continues; the Turkish state's own legal position, articulated in domestic court rulings and to the European Court of Human Rights, corroborates that the problem is considered largely resolved by existing domestic minority-foundation law, i.e. it argues the treaty obligation is discharged, not still operative in the expansive sense.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__expansive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__expansive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__expansive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lausanne_minority_protections__expansive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__expansive_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__expansive_reading_tests).
:- end_tests(lausanne_minority_protections__expansive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28) because under this reading no party is extracting rent through the arrangement — the communities are recovering/retaining self-administration, not capturing value from an outside party. Suppression is moderate (0.42) because the reading's practical force depends on continuous diplomatic and legal advocacy against a state that can simply decline to apply it; the constraint's persistence is contested, not self-executing. Accessibility collapse is moderate (0.45): the expansive reading remains a live, arguable position rather than settled law, so alternatives (the restrictive reading prevailing) have not collapsed. Resistance is elevated (0.55) because Turkish domestic legal and political actors actively contest this reading in courts and administration.
 *
 * DIRECTIONALITY LOGIC:
 *   The minority communities and their schools are beneficiaries under this reading (low d) — the arrangement, if honored, subsidizes their institutional survival rather than extracting from them. The Turkish state occupies the agenda_setter seat with high structural leverage: it administers the domestic legal apparatus that determines whether the expansive reading is honored in practice, and its exit options are best described as arbitrage — it can select whichever reading serves its diplomatic and domestic-political interests at a given moment. No victim group is named because this reading's own logic does not identify anyone being extracted from by successful institutional self-administration; the vulnerability runs the other way — toward the beneficiaries, if the reading loses the interpretive contest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (absorbing Ottoman-era minority self-governance into the new Turkish state without dissolving it) is contested as live vs. dead: the communities and outside minority-law scholars view it as still live and unresolved (Halki still closed after 53 years), while the Turkish state's litigation posture treats the matter as substantially discharged through existing domestic foundation law. This is precisely the kind of live/dead disagreement the R5 interview is built to surface rather than adjudicate — the classification does not resolve whether the expansive reading is 'right,' only whether, taking this reading as authored, the coordination-versus-extraction structure the reading describes computes as a rope (coordination, no captured rent) rather than a snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_continuity_scope_ambiguity,
    'Does ''protection of minorities'' in the Lausanne text encompass institutional self-administration and clergy formation as a matter of treaty obligation, or only individual freedom of worship and non-discrimination, leaving institutional matters to domestic law?',
    'Authoritative international adjudication (e.g., ICJ advisory opinion, or a definitive, uncontested pattern of state practice/acquiescence by Turkey and guarantor states over an extended period) would settle which reading the treaty text supports; absent that, the ambiguity is irreducible and the reading remains contested.',
    'If the restrictive reading is vindicated, this constraint''s coordination function dissolves entirely — the theological schools, community courts, and foundation self-administration this reading protects would have no treaty basis, converting the beneficiary communities into unprotected domestic actors subject to ordinary law with no residual claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_continuity_scope_ambiguity, conceptual, 'Whether Lausanne''s minority-protection guarantee extends to institutional governance or only individual worship rights.').

omega_variable(
    reading_contest_location,
    'Where precisely does the expansive reading diverge from its siblings — is the dispute about WHAT is protected (scope: institutions vs. individuals, per the restrictive reading) or WHO enforces it (mechanism: domestic interpretation vs. guarantor-state/ECHR supervision, per the guarantor reading)?',
    'Track which specific treaty clauses and historical negotiating-history sources each reading cites; the expansive and restrictive readings dispute Article 40''s scope, while the expansive and guarantor readings can coexist on scope but diverge on which forum adjudicates it.',
    'If the dispute is purely about enforcement mechanism (expansive vs. guarantor), the expansive reading could survive with guarantor-state backing even if Turkish domestic courts resist it. If the dispute is about substantive scope (expansive vs. restrictive), guarantor-state involvement cannot rescue the reading — the underlying claim would simply be wrong.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_location, conceptual, 'Whether this reading''s contest with its siblings is over the substantive scope of protection or the enforcement forum.').

omega_variable(
    institutional_survival_versus_extraction,
    'Could the expansive reading''s beneficiary institutions, if fully vindicated, become extractive over time — e.g., community courts adjudicating personal-status matters in ways that trap individual members who might prefer secular civil jurisdiction?',
    'Compare outcomes and exit options for individual community members under community-court jurisdiction versus general civil courts in jurisdictions where the expansive reading is more fully honored (e.g., compare to minority-court arrangements elsewhere in Europe).',
    'If community courts function as identity-locking mechanisms against individual members'' preferences, this reading''s institutions could shift from rope to tangled_rope for those individuals even while remaining a rope at the community-institution level — a possible future decomposition into a separate constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_survival_versus_extraction, empirical, 'Whether institutional self-administration protected by this reading could become extractive toward individual community members.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__expansive_reading, 1923, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1923, lausanne_minority_protections__expansive_reading, theater_ratio, 1923, 0.1).
narrative_ontology:measurement(laus_tr_t1940, lausanne_minority_protections__expansive_reading, theater_ratio, 1940, 0.15).
narrative_ontology:measurement(laus_tr_t1971, lausanne_minority_protections__expansive_reading, theater_ratio, 1971, 0.25).
narrative_ontology:measurement(laus_tr_t1990, lausanne_minority_protections__expansive_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(laus_tr_t2005, lausanne_minority_protections__expansive_reading, theater_ratio, 2005, 0.29).
narrative_ontology:measurement(laus_tr_t2024, lausanne_minority_protections__expansive_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(laus_be_t1923, lausanne_minority_protections__expansive_reading, base_extractiveness, 1923, 0.1).
narrative_ontology:measurement(laus_be_t1940, lausanne_minority_protections__expansive_reading, base_extractiveness, 1940, 0.15).
narrative_ontology:measurement(laus_be_t1971, lausanne_minority_protections__expansive_reading, base_extractiveness, 1971, 0.22).
narrative_ontology:measurement(laus_be_t1990, lausanne_minority_protections__expansive_reading, base_extractiveness, 1990, 0.24).
narrative_ontology:measurement(laus_be_t2005, lausanne_minority_protections__expansive_reading, base_extractiveness, 2005, 0.26).
narrative_ontology:measurement(laus_be_t2024, lausanne_minority_protections__expansive_reading, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1923, lausanne_minority_protections__expansive_reading, suppression_requirement, 1923, 0.2).
narrative_ontology:measurement(laus_su_t1940, lausanne_minority_protections__expansive_reading, suppression_requirement, 1940, 0.3).
narrative_ontology:measurement(laus_su_t1971, lausanne_minority_protections__expansive_reading, suppression_requirement, 1971, 0.4).
narrative_ontology:measurement(laus_su_t1990, lausanne_minority_protections__expansive_reading, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement(laus_su_t2005, lausanne_minority_protections__expansive_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(laus_su_t2024, lausanne_minority_protections__expansive_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__expansive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__expansive_reading, 0.1).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the lausanne_minority_protections kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle: expansive_reading (this file, ε=0.28, rope — coordination framing, low extraction, beneficiaries but no victims), restrictive_reading (expected higher accessibility_collapse toward domestic-law supremacy, likely mountain-adjacent or rope depending on framing of state discretion), and guarantor_reading (expected to emphasize enforcement mechanism and international supervision, likely tangled_rope if guarantor-state leverage is read as creating asymmetric diplomatic dependency). All three share the same treaty text and historical kernel but diverge in what they claim is protected and how; they are linked bidirectionally so contamination/legitimacy shifts in one reading's institutional standing can be traced to pressure on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
