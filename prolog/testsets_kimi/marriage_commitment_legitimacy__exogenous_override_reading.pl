% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__exogenous_override_reading
 *   human_readable: LDS 1890 Manifesto Exogenous Override (Federal Coercion Reading)
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint story models the 1890 Manifesto issued by the LDS Church
 *   under the reading that it represents pure federal coercion forcing
 *   institutional capitulation. Under this reading, the Manifesto is not
 *   prophetic revelation (the endogenous reading) nor strategic prophetic
 *   management (the hybrid reading), but rather the coerced suspension of
 *   plural marriage practice under the Edmunds-Tucker Act and federal
 *   territorial policy. Theological doctrine regarding plural marriage is
 *   held unchanged in theory, while practice is suspended in material
 *   reality, producing a sustained legitimacy crisis for members who
 *   experience a widening gap between spiritual framing and enforced marital
 *   conditions. The federal government is the structural beneficiary,
 *   extracting institutional compliance and national normative uniformity;
 *   the LDS membership and existing polygamous families are the victims,
 *   bearing doctrinal abandonment costs and family dissolution.
 *
 * KEY AGENTS:
 *   - Federal government (agenda_setter/institutional): enforces coerced compliance via territorial law and marshals
 *   - LDS institutional leadership (agenda_setter/payer): issues Manifesto under duress, administers suspension, bears doctrinal contradiction costs
 *   - LDS membership (payer/organized): identity-locked members bear legitimacy crisis and spiritual dissonance
 *   - Polygamous families (payer/powerless): trapped in delegitimized marriages, targeted for dissolution or prosecution
 *   - Protestant national majority (beneficiary/organized): passive beneficiary of enforced monogamous hegemony
 *   - Underground resisters (excluded/powerless): silenced theological dissent excluded from post-Manifesto discourse
 *   - Historian observers (observer/analytical): external corroborators of the exogenous causation narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, 0.88).
domain_priors:suppression_score(marriage_commitment_legitimacy__exogenous_override_reading, 0.82).
domain_priors:theater_ratio(marriage_commitment_legitimacy__exogenous_override_reading, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__exogenous_override_reading, "LDS 1890 Manifesto Exogenous Override (Federal Coercion Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__exogenous_override_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__exogenous_override_reading, 'ebfd0942-69d7-467e-8b38-4b51f547769f').
narrative_ontology:cs_kernel_codification('ebfd0942-69d7-467e-8b38-4b51f547769f', fixed_text).
narrative_ontology:cs_authority_grounding('ebfd0942-69d7-467e-8b38-4b51f547769f', extraction).
narrative_ontology:cs_interpretation_layer_present('ebfd0942-69d7-467e-8b38-4b51f547769f').
narrative_ontology:cs_reading_relation('ebfd0942-69d7-467e-8b38-4b51f547769f', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('ebfd0942-69d7-467e-8b38-4b51f547769f', marriage_commitment_legitimacy__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('ebfd0942-69d7-467e-8b38-4b51f547769f', foundational, federal_coercion_supersedes_religious_practice).
narrative_ontology:cs_axiom_status(federal_coercion_supersedes_religious_practice, holdable).
narrative_ontology:cs_axiom_grounding('ebfd0942-69d7-467e-8b38-4b51f547769f', federal_coercion_supersedes_religious_practice, empirically_contingent).
narrative_ontology:cs_axiom('ebfd0942-69d7-467e-8b38-4b51f547769f', foundational, doctrine_practice_separability_under_duress).
narrative_ontology:cs_axiom_status(doctrine_practice_separability_under_duress, holdable).
narrative_ontology:cs_axiom_grounding('ebfd0942-69d7-467e-8b38-4b51f547769f', doctrine_practice_separability_under_duress, conventional).
narrative_ontology:cs_reference_frame('ebfd0942-69d7-467e-8b38-4b51f547769f', coerced_practical_accommodation).
narrative_ontology:cs_drift_state('ebfd0942-69d7-467e-8b38-4b51f547769f', post_second_manifesto_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ebfd0942-69d7-467e-8b38-4b51f547769f', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, protestant_national_majority).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, polygamous_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_institutional_leadership).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__exogenous_override_reading, federal_supremacy_over_territorial_marriage).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__exogenous_override_reading, monogamous_national_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploys the Edmunds-Tucker Act, federal marshals, and threats of property confiscation and imprisonment to force the LDS Church to abandon plural marriage; enforces the national monogamous marriage standard and extracts institutional compliance and territorial political integration.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, mobile, national).

% Issues the 1890 Manifesto under explicit federal duress, administratively suspending plural marriage while asserting doctrinal validity remains unchanged; bears the cost of doctrinal contradiction, member disillusionment, and institutional humiliation to secure corporate survival and Utah statehood.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_institutional_leadership, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__exogenous_override_reading, lds_institutional_leadership, payer).

% Taught that plural marriage is a divine principle eternally binding, yet forced to abandon the practice under church directive justified by federal pressure; experiences a legitimacy crisis as material conditions directly contradict spiritual framing. Exit means apostasy, excommunication, and social death within a fused religious and communal identity.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership, payer,
    organized, biographical, identity_locked, regional).

% Existing plural marriages delegitimized overnight by the Manifesto; forced into dissolution, hiding, or federal prosecution; bear the direct material, emotional, and spiritual costs of the suspended practice with no compensatory exit.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, polygamous_families, payer,
    powerless, biographical, trapped, local).

% Benefits from the suppression of Mormon plural marriage as validation of monogamous Protestant cultural and political hegemony; enforces the constraint at no direct cost to themselves while gaining territorial and normative integration.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, protestant_national_majority, beneficiary,
    organized, generational, mobile, national).

% Polygamists who continue the practice in secret after 1890 and are excommunicated or prosecuted; their theological arguments that the original revelation remains binding are structurally excluded from post-Manifesto institutional discourse and federal amnesty negotiations.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, underground_resisters, excluded,
    powerless, biographical, trapped, local).

% Analyze the Manifesto as a case of federal coercion producing institutional capitulation; document the persistent gap between official doctrinal claims and enforced marital practice, corroborating the exogenous causation narrative from outside both the benefiting and paying seats.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, historian_observers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the coordination problem of conflicting marriage regimes in the federal territory by subordinating a localized religious plural marriage system to a uniform national monogamous legal standard enforced by federal power.
% TRANSFER_FUNCTION: Moves doctrinal legitimacy and institutional compliance from the LDS community to the federal state apparatus; transfers the spiritual and material costs of family dissolution and doctrinal abandonment onto LDS membership and existing polygamous families.
% ABSENT_VOICES: Underground resisters who continued plural marriage and contested the Manifesto's legitimacy are excluded from institutional discourse; their theological objections are silenced by excommunication and federal prosecution, leaving no formal seat for dissent within the post-1890 church structure.
% DISAPPEARANCE_RATIONALE: If federal coercion vanished and the Manifesto were withdrawn, the LDS institution would face immediate pressure to either restore plural marriage practice or definitively repudiate the doctrine; existing families would reorganize, Utah's statehood bargain would unravel, and the national marriage regime would lose its enforced uniformity in the Mountain West.
% FOUNDING_PROBLEM: The political problem of integrating a territorially concentrated, polygamous religious community into a nation-state legally and culturally committed to monogamous marriage norms and federal legal supremacy over the territories.
% FOUNDING_PROBLEM_CORROBORATION: Federal legislators and territorial governors attest the problem from the coercing seat; LDS dissidents and underground resisters attest that the problem was suppressed rather than solved. Academic historians and political scientists outside the beneficiary set corroborate that statehood was achieved but at the cost of a permanent doctrinal-practice schism.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__exogenous_override_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the constraint extracts doctrinal practice and institutional submission without altering the underlying theological commitment, leaving members to bear unresolved spiritual costs. Suppression is high (0.82) because persistence depends on federal legal coercion and the exclusion of rival marriage practices, not on internal conviction. Theater ratio is high (0.75) because the explicit claim that doctrine remains eternally valid while practice is permanently suspended is a highly performative arrangement sustaining an unsustainable contradiction. Accessibility collapse is substantial (0.72) because exit for identity-locked members means apostasy and social death, while underground alternatives face federal prosecution. Resistance is moderate (0.55) because underground polygamy persisted, but open institutional resistance was crushed by the coercive environment. The measurement series runs on a single shared time grid (1890â1904) showing extraction intensifying as the permanence of the capitulation became clear, theater rising as the doctrine-practice gap widened, and suppression fluctuating as federal enforcement gave way to institutional self-policing before the Second Manifesto renewed coercive pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the federal seat, the constraint is legitimate law enforcement and national marriage standardizationâa necessary coordination function for territorial integration. From the LDS membership seat, the same structure reads as coerced extraction of spiritual practice, where the coordination story is cover for the destruction of a commanded religious form. The institutional leadership experiences both faces simultaneously: they administer the coordination (Manifesto issuance, membership instruction) while paying its doctrinal costs. The engine computes this seat divergence from the identical structural data; the authored claim of tangled_rope encodes that both interpretations are structurally real rather than perspectival illusions.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government and the Protestant national majority sit near the full-beneficiary end: the constraint subsidizes their normative hegemony and territorial control. The LDS membership and polygamous families sit near the full-target end: they pay the extraction directly through doctrinal abandonment and family dissolution. The LDS institutional leadership occupies a mixed positionâformally agenda-setting but under such severe federal constraint that their directionality is pulled toward the target side by their payer costs and constrained exit, despite their administrative role. The engine will compute this divergence from the structural declarations: beneficiaries get damped effective extraction, while victims with identity-locked or trapped exit get amplified effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this constraint as a rope (pure coordination of national marriage norms) by requiring named victims and active enforcement, which forces the asymmetry into the record. It prevents mislabeling as a snare by acknowledging the genuine coordination function the federal government performs: without a uniform marriage standard, territorial legal chaos was a real possibility. The tangled_rope classification captures that the coordination mechanism (national legal uniformity) and the extraction mechanism (coerced doctrinal suspension) are the same structure, not separable components. The high theater ratio (0.75) further signals that the coordination story is increasingly performative, not a rope that has drifted, because the doctrine-practice split was present from inception.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exogenous_vs_endogenous_causation,
    'Was the 1890 Manifesto primarily caused by federal coercion, or did prophetic revelation play an independent causal role?',
    'Archival discovery of federal executive communications to church leadership in 1889â1890, or prophetic diary evidence of independent revelatory timing preceding federal threats.',
    'If independent revelation is documented, this constraint''s Îµ would shift downward and its classification would move toward rope or scaffold; if pure coercion is confirmed, the exogenous_override reading retains its high extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exogenous_vs_endogenous_causation, empirical, 'Historical causation ambiguity between federal coercion and prophetic revelation').

omega_variable(
    doctrine_practice_separability,
    'Can a religious community sustain doctrinal commitment eternally while permanently suspending the commanded practice without collapsing legitimacy?',
    'Longitudinal membership-retention and orthodoxy-adherence data across the 1890â1910 interval, combined with textual analysis of sermons addressing the doctrine-practice gap.',
    'If legitimacy collapsed rapidly, the constraint functioned as a snare extracting compliance until rupture; if members cognitively compartmentalized successfully, the theater ratio overstates the lived extraction and the constraint operated more as tangled rope with temporary coordination for institutional survival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_practice_separability, conceptual, 'Legitimacy sustainability under suspended practice').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is member compliance driven by structural federal threat, or by internalized religious obedience to institutional leadership that has become identity-fused?',
    'Post-Manifesto trajectory analysis: if compliance persisted after federal enforcement visibly relaxed (post-statehood), suppression was partially internalized; if compliance tracked federal threat intensity, suppression was structural.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest, amplifying extraction for identity-locked members; if purely structural, the constraint is more fragile and its classification leans toward scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism for LDS membership').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__exogenous_override_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0, 0.68).
narrative_ontology:measurement(marr_tr_t3, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 3, 0.72).
narrative_ontology:measurement(marr_tr_t6, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 6, 0.76).
narrative_ontology:measurement(marr_tr_t9, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 9, 0.8).
narrative_ontology:measurement(marr_tr_t12, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 12, 0.83).
narrative_ontology:measurement(marr_tr_t14, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 14, 0.85).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 0, 0.82).
narrative_ontology:measurement(marr_be_t3, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 3, 0.8).
narrative_ontology:measurement(marr_be_t6, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 6, 0.83).
narrative_ontology:measurement(marr_be_t9, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 9, 0.85).
narrative_ontology:measurement(marr_be_t12, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 12, 0.87).
narrative_ontology:measurement(marr_be_t14, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 14, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(marr_su_t3, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 3, 0.78).
narrative_ontology:measurement(marr_su_t6, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 6, 0.72).
narrative_ontology:measurement(marr_su_t9, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 9, 0.75).
narrative_ontology:measurement(marr_su_t12, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 12, 0.82).
narrative_ontology:measurement(marr_su_t14, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 14, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__exogenous_override_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_legitimacy__exogenous_override_reading, 0.08).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage_commitment_legitimacy kernel, decomposed per the Îµ-invariance principle. Its siblings (endogenous_reinterpretation_reading and hybrid_pragmatic_reading) are separate constraints with distinct Îµ values and stakeholder directionalities. The kernel decomposes because the same natural-language label (the Manifesto) covers structurally distinct claims: genuine revelation, pure coercion, and strategic management.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
