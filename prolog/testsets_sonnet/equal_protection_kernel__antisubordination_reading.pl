% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__antisubordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__antisubordination_reading, []).

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
 *   constraint_id: equal_protection_kernel__antisubordination_reading
 *   human_readable: Equal Protection Clause — Antisubordination Reading
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   This story instantiates the antisubordination reading of the Equal
 *   Protection Clause: the clause is read as targeting caste-like
 *   subordination of historically oppressed groups rather than racial
 *   classification as such. Under this reading, state action that entrenches
 *   hierarchy is forbidden and state action that dismantles hierarchy —
 *   including race-conscious remedial and integrative measures — is
 *   permitted. The doctrine functions as a coordination mechanism enabling
 *   institutions (school districts, legislatures) to pursue integration and
 *   remedial redistribution without every race-conscious measure being
 *   invalidated, while withdrawing equal-protection standing from
 *   institutions whose historical advantage rested on caste structure. This
 *   is one of three readings of a single contested kernel
 *   (equal_protection_kernel); the colorblind_reading and remedial_reading
 *   are separate constraint stories with their own ε values,
 *   beneficiary/victim sets, and classifications — this story does not
 *   average over them or describe their contest internally.
 *
 * KEY AGENTS:
 *   - historically_subordinated_racial_castes: primary beneficiary (powerless/trapped) — the reading is built around their structural position
 *   - school_districts_pursuing_integration: agenda-setting institutional actor implementing the doctrine
 *   - caste_enforcing_institutions: structural payer — loses the equal-protection shield the colorblind reading would afford
 *   - dominant_group_individual_claimants: excluded voice — objections not cognizable within this reading's framework
 *   - federal_and_state_courts: agenda-setter choosing among kernel readings case by case
 *   - civil_rights_legal_scholars: analytical observer articulating and defending the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, 0.28).
domain_priors:suppression_score(equal_protection_kernel__antisubordination_reading, 0.35).
domain_priors:theater_ratio(equal_protection_kernel__antisubordination_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__antisubordination_reading, rope).
narrative_ontology:human_readable(equal_protection_kernel__antisubordination_reading, "Equal Protection Clause — Antisubordination Reading").
narrative_ontology:topic_domain(equal_protection_kernel__antisubordination_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__antisubordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__antisubordination_reading, '80f11d8b-a49b-4748-a479-dc4155249ac4').
narrative_ontology:cs_kernel_codification('80f11d8b-a49b-4748-a479-dc4155249ac4', fixed_text).
narrative_ontology:cs_authority_grounding('80f11d8b-a49b-4748-a479-dc4155249ac4', practice).
narrative_ontology:cs_interpretation_layer_present('80f11d8b-a49b-4748-a479-dc4155249ac4').
narrative_ontology:cs_reading_relation('80f11d8b-a49b-4748-a479-dc4155249ac4', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('80f11d8b-a49b-4748-a479-dc4155249ac4', equal_protection_kernel__remedial_reading, influences).
narrative_ontology:cs_axiom('80f11d8b-a49b-4748-a479-dc4155249ac4', foundational, clause_targets_caste_not_classification).
narrative_ontology:cs_axiom_status(clause_targets_caste_not_classification, holdable).
narrative_ontology:cs_axiom_grounding('80f11d8b-a49b-4748-a479-dc4155249ac4', clause_targets_caste_not_classification, conventional).
narrative_ontology:cs_axiom('80f11d8b-a49b-4748-a479-dc4155249ac4', secondary, dominant_group_lacks_cognizable_harm_from_remedial_measures).
narrative_ontology:cs_axiom_status(dominant_group_lacks_cognizable_harm_from_remedial_measures, holdable).
narrative_ontology:cs_axiom_grounding('80f11d8b-a49b-4748-a479-dc4155249ac4', dominant_group_lacks_cognizable_harm_from_remedial_measures, deontological).
narrative_ontology:cs_reference_frame('80f11d8b-a49b-4748-a479-dc4155249ac4', reconstruction_antisubordination_purpose).
narrative_ontology:cs_drift_state('80f11d8b-a49b-4748-a479-dc4155249ac4', post_sffa_harvard_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('80f11d8b-a49b-4748-a479-dc4155249ac4', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__antisubordination_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, historically_subordinated_racial_castes).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, school_districts_pursuing_integration).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, affirmative_action_beneficiaries).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, caste_enforcing_institutions).
narrative_ontology:constraint_vindicates(equal_protection_kernel__antisubordination_reading, antisubordination_principle).
narrative_ontology:constraint_vindicates(equal_protection_kernel__antisubordination_reading, state_action_dismantling_hierarchy_is_permitted).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups whose subordination the clause, on this reading, is understood to target — descendants of enslaved and segregated populations, and other groups subjected to caste-like exclusion. Under this reading, race-conscious remedies designed to dismantle the residue of that subordination are permitted state action rather than constitutional violations. Their exit from the subordinated position depends on the state's willingness to use race-conscious tools; they cannot simply opt out of the historical structure they were born into.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, historically_subordinated_racial_castes, beneficiary,
    powerless, generational, trapped, national).

% Local and state education authorities that design race-conscious admissions, redistricting, or resource-allocation policies aimed at reducing racial isolation or its downstream effects. On this reading they may consider race as a tool to dismantle hierarchy without triggering strict scrutiny in the way a hierarchy-entrenching classification would. They are constrained by litigation risk if courts adopt a sibling reading instead.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, school_districts_pursuing_integration, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__antisubordination_reading, school_districts_pursuing_integration, beneficiary).

% Institutions and actors whose historical advantage was built on formal or informal caste structures (e.g., districts that maintained segregative zoning, employers with exclusionary hiring histories). Under this reading, they cannot invoke equal protection to block remedial race-conscious measures, because the clause does not treat their loss of a caste-conferred advantage as cognizable harm. They retain resources and mobility to litigate, lobby, or relocate, but lose the doctrinal shield the colorblind reading would give them.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, caste_enforcing_institutions, payer,
    powerful, biographical, mobile, national).

% Individual members of historically dominant racial groups who feel disadvantaged by a specific race-conscious measure (e.g., a rejected magnet-school applicant). Under this reading their claims are structurally weaker because the clause is not read as protecting against loss of caste-position benefits; they are not in the doctrinal room this reading builds for — their objections are heard, if at all, through the sibling colorblind or remedial readings, not this one.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, dominant_group_individual_claimants, excluded,
    moderate, biographical, constrained, national).

% Courts applying this reading must distinguish hierarchy-entrenching state action (forbidden) from hierarchy-dismantling state action (permitted), a line-drawing exercise absent under the colorblind reading. Their choice of reading determines which of the three kernel readings governs a given case; adopting this reading requires an evidentiary inquiry into subordination that the colorblind reading dispenses with entirely.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, federal_and_state_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Scholars and litigators who articulate and defend the antisubordination reading as the doctrinally and historically correct interpretation of the clause's post-Reconstruction purpose, contesting the colorblind reading's ahistoricism and the remedial reading's narrower tailoring requirements.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, civil_rights_legal_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__antisubordination_reading, diffuse).
narrative_ontology:fixing_cost_class(equal_protection_kernel__antisubordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a doctrinal standard letting the state act affirmatively to dismantle entrenched racial hierarchy — school integration, targeted remedial programs, redistricting to prevent vote dilution — without every such measure being struck down as itself an equal-protection violation.
% TRANSFER_FUNCTION: Reallocates institutional resources, admissions slots, and political representation toward groups the reading identifies as subordinated castes, and correspondingly withdraws the doctrinal shield that would otherwise let caste-advantaged institutions block such reallocation via equal-protection litigation.
% ABSENT_VOICES: Individual claimants from historically dominant groups who believe they bear concrete costs from specific race-conscious measures are structurally outside this reading's protective logic — the reading treats their objection as a category error (mistaking hierarchy-dismantling for hierarchy-entrenching), not a competing equity claim to be weighed.
% DISAPPEARANCE_RATIONALE: If courts abandoned the antisubordination reading, race-conscious remedial programs would lose one of their strongest doctrinal groundings, though the remedial reading could still support narrower diversity-interest measures; caste-enforcing institutions and colorblind-reading advocates would say the world is unchanged or improved (principle finally applied consistently), while civil rights advocates would say the world rearranges sharply as integration and remedial tools lose constitutional cover.
% FOUNDING_PROBLEM: The Fourteenth Amendment was enacted to dismantle the legal and social structure of racial caste established under slavery and continued through Black Codes — the founding problem was not classification per se but the reconstitution of a subordinate racial caste through facially neutral or race-conscious state law.
% FOUNDING_PROBLEM_CORROBORATION: Reconstruction-era congressional debate records and historians of the Fourteenth Amendment (outside any party currently litigating equal protection doctrine) attest that the amendment's drafters targeted caste subordination specifically, not classification neutrality; contemporary colorblind-reading proponents dispute that this original purpose should control present doctrine, making the founding-problem's continued relevance a live doctrinal dispute rather than settled history.
narrative_ontology:disappearance_verdict(equal_protection_kernel__antisubordination_reading, contested).
narrative_ontology:founding_problem_status(equal_protection_kernel__antisubordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__antisubordination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_kernel__antisubordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__antisubordination_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__antisubordination_reading_tests).
:- end_tests(equal_protection_kernel__antisubordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is modest (0.28) because the reading's core function is coordinative — it licenses remedial state action rather than mandating transfer from any specific payer class in the way a snare would. Suppression (0.35) reflects the doctrinal foreclosure of equal-protection claims by caste-advantaged institutions against remedial measures, which is real but bounded (they retain political and litigation avenues, just not this specific doctrinal shield). Resistance is high (0.72) because this reading is fiercely contested by colorblind-reading proponents and has never commanded a majority on the Supreme Court, making it a doctrine defended primarily in scholarship and dissent rather than settled controlling law. Theater ratio is low-to-moderate and rising slightly, reflecting increasing invocation of the reading in advocacy and litigation briefs relative to its actual controlling weight in majority opinions. Suppression trends downward over the historical interval as courts have increasingly constrained race-conscious remedies (Croson, Adarand, SFFA v. Harvard), meaning the doctrine's real-world suppressive force against caste-enforcing institutions has weakened even as scholarly articulation of the reading has if anything intensified.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of historically subordinated castes and integrating school districts, this reading looks like a rope: a coordination device correcting a structural defect that would otherwise let every remedial measure be struck down as its own equal-protection violation. From the seat of caste-enforcing institutions, the same doctrine looks like a tangled rope or worse: a structure that coordinates state remedial capacity while extracting the specific doctrinal protection they previously held. The engine computes these divergent seat-level readings from the declared beneficiary/victim/exit structure; the claimed_type of rope reflects the coordination function as authored, and the divergence itself is the analytically interesting output, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated castes are the clearest beneficiaries: the doctrine exists to license state action on their behalf and derives its entire justification from their structural position, so directionality sits near the full-beneficiary end. Caste-enforcing institutions sit near the target end because, under this reading specifically, they lose the ability to claim equal-protection harm from losing caste-conferred advantage — a doctrinal position the colorblind reading would reverse entirely. Dominant-group individual claimants are excluded rather than positioned as victims proper, because their objections are not that they suffer subordination (the clause's stated target) but that they suffer disadvantage from a remedial measure — a categorically different harm this reading does not recognize as cognizable. This is the precise structural delta from the remedial reading, which would recognize their claim but subject the remedy to narrow tailoring, and from the colorblind reading, which would treat their claim as fully cognizable regardless of remedial purpose.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — dismantling caste subordination reconstituted through facially neutral law after Reconstruction — remains contested rather than resolved: proponents of this reading argue the problem persists (racially disparate outcomes tracing to historical exclusion), while colorblind-reading proponents argue the problem the clause was built for (formal legal subordination) has been solved and continued race-conscious remedy is now itself hierarchy-entrenching in reverse. The antisubordination reading resists the mandatrophy label directly by defining permitted action as precisely that which reduces hierarchy — a self-limiting principle in theory, though its critics contend it has in practice been invoked to defend permanent racial classification, which is exactly the disagreement the founding_problem_status field is built to surface rather than adjudicate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subordination_versus_classification_boundary,
    'Is there a principled, judicially administrable line between state action that ''dismantles hierarchy'' (permitted under this reading) and state action that merely reclassifies by race in ways a colorblind reading would forbid regardless of intent?',
    'Doctrinal analysis of how courts adopting this reading have distinguished the two categories across cases (e.g., school integration plans upheld versus quota-like set-asides struck down even under sympathetic readings), and empirical study of whether the distinction tracks measurable subordination indicators or judicial discretion.',
    'If no administrable line exists, the reading collapses into unreviewable judicial discretion about which race-conscious measures ''count'' as remedial, which would support the colorblind_reading''s categorical-rule critique; if a workable line exists, the antisubordination reading is more defensible as a stable doctrinal standard rather than a results-oriented exception.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_versus_classification_boundary, conceptual, 'Whether the hierarchy-entrenching/dismantling distinction is doctrinally administrable or discretion in disguise.').

omega_variable(
    kernel_reading_selection_mechanism,
    'Which of the three sibling readings (antisubordination, remedial, colorblind) actually controls in a given case, and what determines a court''s choice among them — text, precedent, composition of the bench, or something else?',
    'Track Supreme Court and circuit court voting patterns across equal-protection cases against the reading each majority/dissent opinion explicitly or implicitly adopts, correlated with appointing administration and prior circuit precedent.',
    'If reading-selection tracks judicial composition rather than principled textual or historical argument, all three readings (including this one) are better understood as contested political commitments wearing constitutional-interpretation clothing, which would elevate the type_class of several related omegas from empirical to conceptual/preference.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, empirical, 'Empirical predictors of which kernel reading controls a given equal-protection case.').

omega_variable(
    caste_versus_individual_harm_commensurability,
    'Is the harm this reading declines to recognize (a dominant-group individual''s loss of a specific benefit to a remedial measure) genuinely incommensurable with caste subordination, or is it a real individual harm this reading simply weighs as lower priority?',
    'Philosophical and legal analysis of whether equal-protection harm is properly assessed at the individual or group level, informed by the historical purpose evidence already contested in the founding_problem fields.',
    'If the excluded individual harm is commensurable and simply outweighed, the reading is a policy balancing act dressed as categorical doctrine; if genuinely incommensurable (different kind of harm entirely), the reading''s exclusion of dominant-group individual claims is more structurally principled than a mere balancing choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(caste_versus_individual_harm_commensurability, preference, 'Whether excluded individual-harm claims are incommensurable with group subordination or merely outweighed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__antisubordination_reading, 1868, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1868, equal_protection_kernel__antisubordination_reading, theater_ratio, 1868, 0.1).
narrative_ontology:measurement(equa_tr_t1954, equal_protection_kernel__antisubordination_reading, theater_ratio, 1954, 0.12).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__antisubordination_reading, theater_ratio, 1978, 0.15).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_kernel__antisubordination_reading, theater_ratio, 2003, 0.17).
narrative_ontology:measurement(equa_tr_t2016, equal_protection_kernel__antisubordination_reading, theater_ratio, 2016, 0.19).
narrative_ontology:measurement(equa_tr_t2026, equal_protection_kernel__antisubordination_reading, theater_ratio, 2026, 0.2).

% Extraction over time
narrative_ontology:measurement(equa_be_t1868, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1868, 0.15).
narrative_ontology:measurement(equa_be_t1954, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1954, 0.18).
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1978, 0.22).
narrative_ontology:measurement(equa_be_t2003, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2003, 0.24).
narrative_ontology:measurement(equa_be_t2016, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2016, 0.26).
narrative_ontology:measurement(equa_be_t2026, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2026, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1868, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1868, 0.55).
narrative_ontology:measurement(equa_su_t1954, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1954, 0.5).
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1978, 0.42).
narrative_ontology:measurement(equa_su_t2003, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2003, 0.38).
narrative_ontology:measurement(equa_su_t2016, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2016, 0.36).
narrative_ontology:measurement(equa_su_t2026, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2026, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__antisubordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, remedial_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraint stories decomposing the natural-language concept 'the Equal Protection Clause' per the ε-invariance principle: colorblind_reading (categorical prohibition on racial classification regardless of purpose), remedial_reading (narrow tailoring to documented historical exclusion or compelling interest), and this antisubordination_reading (targets caste subordination, not classification per se). Each reading has a distinct beneficiary/victim structure, a distinct ε, and a distinct classification; they are linked via network.affects_constraints rather than merged into one story, since forcing one story to carry all three readings would violate ε-invariance (measuring 'the clause' under each reading yields structurally different extraction and suppression profiles).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
