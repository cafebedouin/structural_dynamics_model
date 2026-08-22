% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__mandatory_interpretive_discretion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__mandatory_interpretive_discretion, []).

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
 *   constraint_id: balfour_mandate_instruments__mandatory_interpretive_discretion
 *   human_readable: British Mandatory Interpretive Discretion in Palestine Mandate
 *   domain: international/political/colonial
 *
 * SUMMARY:
 *   The British mandatory power over Palestine (1920–1948) possessed sole
 *   interpretive authority over the Palestine Mandate instrument without
 *   external review mechanism or appellate process. This constraint is ONE
 *   READING of the contested kernel balfour_mandate_instruments: the reading
 *   that emphasizes the mandatory power's discretion itself as the operative
 *   extraction mechanism, independent of how that discretion is deployed.
 *   Both Arab and Jewish Palestinian communities faced path-dependent lock-in
 *   where each reinterpretation of the mandate (1920 pro-Zionist period; 1930
 *   shift toward Arab accommodation; 1939 White Paper restricting immigration
 *   and land transfer) reset the baseline for subsequent negotiations.
 *   Neither community could appeal to fixed textual meaning—the text was
 *   re-read to fit metropolitan political interests. The British
 *   administration benefited from policy flexibility and divide-and-rule
 *   dynamics; both Palestinian communities bore the cost of strategic
 *   uncertainty and inability to plan long-term political or economic
 *   strategy.
 *
 * KEY AGENTS:
 *   - british_mandatory_administration: Controls interpretation of mandate without external check; benefits from policy flexibility and the strategic uncertainty it imposes
 *   - arab_palestinian_communities: Face land dispossession and policy unpredictability; identity-locked by land ties; unable to appeal interpretations
 *   - jewish_palestinian_communities: Benefit from some interpretive shifts, constrained by others; also face strategic uncertainty and inability to predict future policy direction
 *   - league_of_nations: Nominally oversees but possesses no machinery for interpreting disputes or enforcing rulings
 *   - excluded_international_arbitration: World Court, PCIJ, and other legal mechanisms exist but are structurally barred from Palestine mandate disputes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.68).
domain_priors:suppression_score(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.72).
domain_priors:theater_ratio(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, extractiveness, 0.68).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__mandatory_interpretive_discretion, snare).
narrative_ontology:human_readable(balfour_mandate_instruments__mandatory_interpretive_discretion, "British Mandatory Interpretive Discretion in Palestine Mandate").
narrative_ontology:topic_domain(balfour_mandate_instruments__mandatory_interpretive_discretion, "international/political/colonial").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__mandatory_interpretive_discretion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__mandatory_interpretive_discretion, '96d8dcf6-2c1f-4e29-b302-abadc8634a15').
narrative_ontology:cs_kernel_codification('96d8dcf6-2c1f-4e29-b302-abadc8634a15', formalized).
narrative_ontology:cs_authority_grounding('96d8dcf6-2c1f-4e29-b302-abadc8634a15', extraction).
narrative_ontology:cs_interpretation_layer_present('96d8dcf6-2c1f-4e29-b302-abadc8634a15').
narrative_ontology:cs_reading_relation('96d8dcf6-2c1f-4e29-b302-abadc8634a15', balfour_mandate_instruments__jewish_national_home_primacy, coexists_with).
narrative_ontology:cs_reading_relation('96d8dcf6-2c1f-4e29-b302-abadc8634a15', balfour_mandate_instruments__dual_obligation_indigenous_rights, coexists_with).
narrative_ontology:cs_axiom('96d8dcf6-2c1f-4e29-b302-abadc8634a15', foundational, mandatory_interpretive_discretion_required).
narrative_ontology:cs_axiom_status(mandatory_interpretive_discretion_required, holdable).
narrative_ontology:cs_axiom_grounding('96d8dcf6-2c1f-4e29-b302-abadc8634a15', mandatory_interpretive_discretion_required, deontological).
narrative_ontology:cs_axiom('96d8dcf6-2c1f-4e29-b302-abadc8634a15', foundational, external_review_precluded).
narrative_ontology:cs_axiom_status(external_review_precluded, holdable).
narrative_ontology:cs_axiom_grounding('96d8dcf6-2c1f-4e29-b302-abadc8634a15', external_review_precluded, conventional).
narrative_ontology:cs_reference_frame('96d8dcf6-2c1f-4e29-b302-abadc8634a15', mandate_discretionary_governance_regime).
narrative_ontology:cs_drift_state('96d8dcf6-2c1f-4e29-b302-abadc8634a15', id_1948_mandate_termination, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('96d8dcf6-2c1f-4e29-b302-abadc8634a15', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_mandatory_administration).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_palestinian_communities).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, jewish_palestinian_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, jewish_palestinian_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the League of Nations mandate to govern Palestine and interpret the mandate instrument's terms. Exercises sole authority to adjudicate between competing readings of the 'national home' clause without external review mechanism or appellate process. Benefits from policy flexibility that allows accommodation of opposing interests through selective interpretation—can shift land policy (1920 vs 1940 regimes), immigration policy (restrictive in White Papers 1922/1930/1939), and institutional recognition based on metropolitan political shifts or security assessments, while claiming fidelity to mandate text. The discretion itself is the instrument of control.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, british_mandatory_administration, agenda_setter,
    institutional, generational, arbitrage, regional).

% Face systematic land dispossession under Ottoman-era property transfers (1920s) and British licensing schemes that facilitate Jewish acquisition, justified by the mandatory power's reading of the mandate as permitting 'national home' development. Unable to appeal interpretations to external authority (League of Nations offers no adjudication mechanism for mandate disputes). Strategic uncertainty over future policy direction (will next White Paper restrict or expand Jewish immigration? will land-transfer regulations reverse?) creates inability to make long-term economic or political plans. Identity locked by ties to ancestral land and community networks; exit means displacement.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_palestinian_communities, payer,
    moderate, generational, identity_locked, regional).

% Benefit from British discretionary choices that facilitate land acquisition and institutional development (Jewish Agency recognition, immigration facilitation in early 1920s periods), but face policy reversals (White Papers of 1930 and 1939 restrict immigration and land transfer). Cannot predict whether the mandatory power will read the mandate as requiring facilitation of 'national home' (pro-Zionist readings) or as subordinating it to protection of existing Arab rights (pro-Arab readings). The same interpretive discretion that sometimes favors Jewish institutional development can be turned toward restriction. Exit constrained by identity investment in the project and by international geography (Diaspora communities have limited nearby alternatives).
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, jewish_palestinian_communities, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__mandatory_interpretive_discretion, jewish_palestinian_communities, beneficiary).

% Nominally oversees mandate administration but possesses no machinery for interpreting mandate instruments, no appellate body, and no enforcement mechanism short of mandate revocation (which has never been used). Functions as an absent authority whose absence is itself the operative constraint—both Palestinian communities appeal to League machinery but find no channel for dispute resolution.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, league_of_nations, observer,
    institutional, generational, analytical, global).

% International legal mechanisms for dispute resolution, treaty interpretation, or minority-rights protection exist elsewhere (World Court, Permanent Court of International Justice) but are structurally barred from Palestine mandate disputes. The mandate instrument contains no arbitration clause, no dispute-resolution procedure, and no mechanism for external review of the mandatory power's interpretation. This exclusion is the structural feature that makes the constraint work—interpretive discretion persists only because no external check exists.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, excluded_international_arbitration, excluded,
    institutional, generational, trapped, global).

% Shifts the Palestine administration's interpretive line through London-based policy changes (pro-Zionist orientation ~1920, pro-Arab reorientation after 1930 Arab violence and Arab League pressure, restrictionist stance 1939 White Paper in face of Nazi threat and refugee crisis). The mandatory power's administrators respond to metropolitan pressure, not to Palestinian appeals. Each metropolitan shift re-anchors the interpretation of the mandate instrument itself.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, metropolitan_british_politics, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__mandatory_interpretive_discretion, metropolitan_british_politics, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. The constraint solves no coordination problem for the communities governed; it is a pure extraction regime.
% TRANSFER_FUNCTION: Transfers control over policy (land, immigration, institutional recognition) from Palestinian communities to the British mandatory administration. The transfer is effected through unilateral interpretive authority without external review or appellate constraint.
% ABSENT_VOICES: International arbitration and law enforcement machinery (World Court, League dispute-resolution procedures) are structurally absent. Palestinian communities appeal to the League of Nations but find no adjudication mechanism; they are excluded from participating in interpretation of the mandate text they are governed by.
% DISAPPEARANCE_RATIONALE: If the mandatory power lost interpretive discretion (through League Court jurisdiction, external arbitration, or fixed textual constraint on policy), both Palestinian communities would reorganize around fixed terms rather than policy unpredictability. Land markets, immigration flows, and institutional development would follow explicit rules instead of White Paper reinterpretations. The strategic uncertainty that locks both communities in place would dissolve.
% FOUNDING_PROBLEM: Governance of a post-Ottoman Palestinian territory under a League of Nations mandate while honoring incompatible British wartime commitments: the Balfour Declaration (commitment to facilitate a Jewish national home in Palestine) and earlier commitments to Arab independence and protection of existing Arab populations.
% FOUNDING_PROBLEM_CORROBORATION: British administrators and metropolitan politicians argue the founding problem remains live—the mandate's contradictions genuinely cannot be resolved without discretionary rebalancing based on circumstances. Palestinian communities (both Arab and Jewish) argue by the 1930s that the founding problem has been misdefined: the problem was not how to reconcile the commitments, but how to determine which commitment takes priority. The discretionary regime avoids that determination and thus perpetuates the problem. Independent scholars and League officials sympathetic to Arab Palestinian interests argue the mandate was solvable under a fixed reading that prioritized existing rights (supporting the dual_obligation reading) or Jewish institutional interests (supporting the jewish_national_home reading), but that discretion prevents resolution.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__mandatory_interpretive_discretion, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__mandatory_interpretive_discretion, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(balfour_mandate_instruments__mandatory_interpretive_discretion, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is high because the mandatory power's interpretive discretion produces path-dependent lock-in: each policy shift resets expectations and prevents communities from making long-term commitments or strategic plans. The discretion itself is the extraction mechanism—not a particular policy choice, but the fact that policy can be rewritten unilaterally. Suppression requirement rises from 0.48 (1920) to 0.82 (1936, during Arab Revolt) because both communities increasingly attempt to resist or appeal the interpretive regime; the mandatory power must intensify enforcement to maintain its discretionary authority as violence escalates and demands for fixed terms mount. Theater ratio (0.41 at end) reflects a significant performative component: the mandatory power publishes White Papers and consultation procedures (1922, 1930, 1939) that simulate deliberation while the interpretive authority remains wholly within the executive. The measurements share a single time grid across all three metrics to enable temporal drift detection; the grid runs 1920–1948 to capture the full mandate period, with points at key policy inflection moments (1925 pro-Zionist expansion, 1930 Arab accommodation, 1936 Arab Revolt, 1939 White Paper shift, 1948 mandate termination).
 *
 * PERSPECTIVAL GAP:
 *   From the British mandatory administration's seat, the arrangement is interpretive discretion justified by administrative complexity and metropolitan interest management. From the Arab Palestinian seat, it is systematic expropriation justified by legal formalism: the same mandate text is re-read to enable land transfer, immigration, and institutional dominance of the Jewish community. From the Jewish Palestinian seat, it is structural uncertainty about whether the next reading will facilitate or restrict the 'national home' development project. The engine computes these divergent classifications from the structural data—the authority asymmetry (only the mandatory power interprets), the exit constraints (both communities are geographically and identity-locked), and the beneficiary structure (the administration benefits from discretion; both communities bear its costs).
 *
 * DIRECTIONALITY LOGIC:
 *   The british_mandatory_administration holds directionality near 0.0 (full beneficiary): it sets the interpretive rules, benefits from policy flexibility, and faces no external constraint or appellate process. Its exit options are arbitrage-level—it can choose to interpret the mandate differently based on metropolitan interests, military pressures, or strategic preference. Arab_palestinian_communities hold directionality near 1.0 (full target): they bear the costs of dispossession under discretionary land policy, face strategic uncertainty, and cannot appeal or reframe the interpretation. Their exit is identity_locked—tied to ancestral land and community networks; displacement is the only exit. Jewish_palestinian_communities hold intermediate directionality (~0.5–0.6): they benefit from some interpretive choices (1920s pro-Zionist readings facilitating land acquisition and immigration) but are constrained by others (1930 and 1939 shifts toward restriction). Their exit is constrained but not identity-locked at the individual level (institutional exit exists for organizations, though it comes at high cost). The beneficiary structure isolates the mandatory power; the victim structure encompasses both Palestinian communities despite their opposed interests.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy—the founding problem (governance of a territory with incompatible claims) has not been solved by the discretionary system, but rather perpetuated by it. Both communities recognize by the 1930s that British discretion is the problem, not the solution. Arab Palestinian demands shift from requests for policy change to demands for self-determination and removal of the mandate. Jewish Palestinian demands shift from request for facilitation to demand for unilateral state-building. The White Papers (1930, 1939) are attempts by the mandatory power to reframe its interpretive authority as constraint on the Zionist project, but they do not resolve the mandate's core mandate—they simply reposition extraction: instead of extracting from the Arab community through land transfers, the administration extracts through restriction of immigration and Jewish institutional development, maintaining strategic uncertainty for both communities. The theater ratio rises from 0.22 to 0.48 across the interval, indicating growing performative component as the actual interpretive discretion shifts from administrative flexibility to divide-and-rule enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretion_vs_policy_extraction,
    'Is the constraint''s extractiveness a function of the mandatory power''s interpretive discretion itself, or of the specific policies chosen through that discretion?',
    'Counterfactual: would a mandatory power with identical discretion authority but fixed (non-oscillating) policy produce the same extraction and resistance? Would fixed interpretations (even if favoring one community) produce lower theater_ratio than oscillating White Papers?',
    'If discretion itself is the extraction mechanism, the constraint is a snare even under ''fair'' interpretations. If the extraction is policy-specific, a mandatory power exercising discretion more neutrally might lower extractiveness. This distinction determines whether the sibling readings (jewish_national_home_primacy, dual_obligation_indigenous_rights) would be less extractive if one were institutionally privileged over the other—or whether the problem is the discretionary regime itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_vs_policy_extraction, conceptual, 'Whether extraction stems from discretion-as-such or from discretion''s deployment.').

omega_variable(
    identity_lock_mechanism_interpersonal,
    'Is the arab_palestinian_communities'' exit-constraint (identity_locked) a function of geographic land ties, or of identity fusion with the Palestinian nationalist project itself?',
    'Post-mandate trajectories: did Palestinian communities reconstitute political organization and economic strategy outside the mandate territory? To what degree did displacement narratives center land loss vs. nationalist project continuation?',
    'If identity-lock is primarily geographic (tied to land), the constraint is territorial expropriation. If primarily nationalist (tied to the independence project), the constraint is structural uncertainty about the terms of that project. The distinction affects whether the suppression axis should track enforcement of land transfer or enforcement of political status determination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_interpersonal, empirical, 'What binds the Arab community to Palestine—geographic ties, nationalist identity, or both inseparably?').

omega_variable(
    league_machinery_as_absent_authority,
    'Is the League of Nations'' absence of interpretive machinery a design feature of the mandate system, or a contingent institutional gap that could have been filled?',
    'Historical record: did mandate drafters intentionally avoid dispute-resolution mechanisms for Palestine, or did they omit them as generic procedure across all mandates? Were there contemporary proposals for League Court jurisdiction that were rejected?',
    'If designed absence, the constraint is intentionally extractive at the structural level—the mandatory power wanted discretion and the League accepted it. If contingent gap, the constraint is structurally extractive through institutional drift. This affects whether sibling readings (jewish_national_home_primacy, dual_obligation_indigenous_rights) could have been institutionally anchored via League machinery (foreclosing mandatory_interpretive_discretion) or whether the discretionary regime was structural to the mandate from inception.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(league_machinery_as_absent_authority, empirical, 'Whether absence of League dispute-resolution was intentional or contingent.').

omega_variable(
    british_interest_alignment_with_discretion,
    'Did the British mandatory power benefit from discretion because it enabled genuine governance of an intractable conflict, or because it enabled extraction of imperial benefit (avoiding commitment, maintaining divide-and-rule, deferring political settlement)?',
    'Metropolitan-level evidence: do British decision-making records frame discretion as administrative necessity, or as strategic tool? Do policy shifts correlate with metropolitan political interests (support for Zionism during WWI/immediate postwar, shift toward Arab accommodation after 1930 Arab violence, restrictionism after 1933 Nazi rise and refugee crisis)?',
    'If discretion was genuinely necessary for governance, the snare classification might be contested by the mandatory power as a rope of complex coordination. If discretion correlated with metropolitan interest rather than Palestinian governance, the snare classification is reinforced. This affects the commentary on whether the founding problem was genuinely intractable or whether the mandatory power chose discretion to avoid difficult boundary-setting.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(british_interest_alignment_with_discretion, empirical, 'Whether British discretion served governance necessity or imperial interest management.').

omega_variable(
    sibling_reading_foreclosure,
    'Would institutionalizing either the ''jewish_national_home_primacy'' or ''dual_obligation_indigenous_rights'' reading logically foreclose this reading of mandatory interpretive discretion, or merely shift the constraint to a different baseline?',
    'Structural analysis: if one reading were fixed in the mandate instrument with external enforcement (e.g., League Court jurisdiction over interpretations), would discretion disappear? Or would it simply shift to interpretation of the fixed reading (secondary discretion)?',
    'If either sibling reading, when institutionalized, would eliminate the interpretive regime entirely, then mandatory_interpretive_discretion genuinely forecloses the others in the same framework—the readings are logically incompatible at the structural level. If secondary discretion would persist, the readings coexist even when one is formally privileged. This is the critical test for relation type in cs_structure.reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether the readings are logically foreclosed or merely demoted by institutionalizing a sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__mandatory_interpretive_discretion, 1920, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1920, 0.22).
narrative_ontology:measurement(balf_tr_t1925, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1925, 0.28).
narrative_ontology:measurement(balf_tr_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1930, 0.35).
narrative_ontology:measurement(balf_tr_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1936, 0.43).
narrative_ontology:measurement(balf_tr_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1939, 0.48).
narrative_ontology:measurement(balf_tr_t1948, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1948, 0.41).

% Extraction over time
narrative_ontology:measurement(balf_be_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1920, 0.52).
narrative_ontology:measurement(balf_be_t1925, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1925, 0.61).
narrative_ontology:measurement(balf_be_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1930, 0.65).
narrative_ontology:measurement(balf_be_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1936, 0.71).
narrative_ontology:measurement(balf_be_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1939, 0.68).
narrative_ontology:measurement(balf_be_t1948, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1948, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1920, 0.48).
narrative_ontology:measurement(balf_su_t1925, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1925, 0.58).
narrative_ontology:measurement(balf_su_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1930, 0.71).
narrative_ontology:measurement(balf_su_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1936, 0.82).
narrative_ontology:measurement(balf_su_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1939, 0.78).
narrative_ontology:measurement(balf_su_t1948, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1948, 0.72).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1920, tn=1948
narrative_ontology:measurement(balf_grid_01, balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse(class), 1920, 0.35).
narrative_ontology:measurement(balf_grid_02, balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse(class), 1948, 0.65).
narrative_ontology:measurement(balf_grid_03, balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse(individual), 1920, 0.38).
narrative_ontology:measurement(balf_grid_04, balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse(individual), 1948, 0.68).
narrative_ontology:measurement(balf_grid_05, balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse(organizational), 1920, 0.42).
narrative_ontology:measurement(balf_grid_06, balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse(organizational), 1948, 0.72).
narrative_ontology:measurement(balf_grid_07, balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse(structural), 1920, 0.71).
narrative_ontology:measurement(balf_grid_08, balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse(structural), 1948, 0.82).
narrative_ontology:measurement(balf_grid_09, balfour_mandate_instruments__mandatory_interpretive_discretion, resistance(class), 1920, 0.64).
narrative_ontology:measurement(balf_grid_10, balfour_mandate_instruments__mandatory_interpretive_discretion, resistance(class), 1948, 0.76).
narrative_ontology:measurement(balf_grid_11, balfour_mandate_instruments__mandatory_interpretive_discretion, resistance(individual), 1920, 0.42).
narrative_ontology:measurement(balf_grid_12, balfour_mandate_instruments__mandatory_interpretive_discretion, resistance(individual), 1948, 0.78).
narrative_ontology:measurement(balf_grid_13, balfour_mandate_instruments__mandatory_interpretive_discretion, resistance(organizational), 1920, 0.58).
narrative_ontology:measurement(balf_grid_14, balfour_mandate_instruments__mandatory_interpretive_discretion, resistance(organizational), 1948, 0.82).
narrative_ontology:measurement(balf_grid_15, balfour_mandate_instruments__mandatory_interpretive_discretion, resistance(structural), 1920, 0.48).
narrative_ontology:measurement(balf_grid_16, balfour_mandate_instruments__mandatory_interpretive_discretion, resistance(structural), 1948, 0.62).
narrative_ontology:measurement(balf_grid_17, balfour_mandate_instruments__mandatory_interpretive_discretion, stakes_inflation(class), 1920, 0.38).
narrative_ontology:measurement(balf_grid_18, balfour_mandate_instruments__mandatory_interpretive_discretion, stakes_inflation(class), 1948, 0.79).
narrative_ontology:measurement(balf_grid_19, balfour_mandate_instruments__mandatory_interpretive_discretion, stakes_inflation(individual), 1920, 0.32).
narrative_ontology:measurement(balf_grid_20, balfour_mandate_instruments__mandatory_interpretive_discretion, stakes_inflation(individual), 1948, 0.81).
narrative_ontology:measurement(balf_grid_21, balfour_mandate_instruments__mandatory_interpretive_discretion, stakes_inflation(organizational), 1920, 0.48).
narrative_ontology:measurement(balf_grid_22, balfour_mandate_instruments__mandatory_interpretive_discretion, stakes_inflation(organizational), 1948, 0.85).
narrative_ontology:measurement(balf_grid_23, balfour_mandate_instruments__mandatory_interpretive_discretion, stakes_inflation(structural), 1920, 0.62).
narrative_ontology:measurement(balf_grid_24, balfour_mandate_instruments__mandatory_interpretive_discretion, stakes_inflation(structural), 1948, 0.88).
narrative_ontology:measurement(balf_grid_25, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression(class), 1920, 0.48).
narrative_ontology:measurement(balf_grid_26, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression(class), 1948, 0.72).
narrative_ontology:measurement(balf_grid_27, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression(individual), 1920, 0.35).
narrative_ontology:measurement(balf_grid_28, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression(individual), 1948, 0.68).
narrative_ontology:measurement(balf_grid_29, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression(organizational), 1920, 0.52).
narrative_ontology:measurement(balf_grid_30, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression(organizational), 1948, 0.76).
narrative_ontology:measurement(balf_grid_31, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression(structural), 1920, 0.68).
narrative_ontology:measurement(balf_grid_32, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression(structural), 1948, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__mandatory_interpretive_discretion, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.12).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments__jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments__dual_obligation_indigenous_rights).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the balfour_mandate_instruments kernel. The sibling constraints instantiate alternative interpretations of the mandate's substantive obligations (jewish_national_home_primacy reads the mandate as directing proto-state development; dual_obligation_indigenous_rights reads it as imposing equal/superior protection of Arab rights). This reading focuses on the STRUCTURE of interpretive authority rather than its content: the constraint is mandatory_interpretive_discretion—the regime under which the mandatory power adjudicates between the siblings without external review. All three constraints share the kernel (the mandate text) but emit different constraint structures depending on which reading is instantiated. The three ε values differ substantially: jewish_national_home_primacy reads as snare with high extraction toward Arab communities (~0.72–0.75 estimated); dual_obligation_indigenous_rights reads as snare with asymmetric extraction toward Jewish communities or as tangled_rope if the dual obligation is read as genuine coordination (~0.60–0.68 estimated); mandatory_interpretive_discretion reads as snare with extraction toward both communities through strategic uncertainty (~0.68). The difference is not measurement perspective—it is structural: each reading instantiates a different constraint with a different beneficiary/victim structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
