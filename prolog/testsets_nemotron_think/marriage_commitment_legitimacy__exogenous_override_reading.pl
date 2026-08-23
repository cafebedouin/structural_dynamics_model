% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: marriage_commitment_legitimacy__exogenous_override_reading
 *   human_readable: 1890 Manifesto as Federal Coercion Forcing LDS Capitulation
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   The 1890 Manifesto (Official Declaration 1) ended the LDS Church's public
 *   practice of plural marriage under sustained federal coercion: the Edmunds
 *   Act (1882) criminalized cohabitation, the Edmunds-Tucker Act (1887)
 *   disincorporated the Church and seized its assets. Wilford Woodruff issued
 *   the Manifesto after the Supreme Court upheld the Edmunds-Tucker Act (Late
 *   Corporation of the Church v. United States, 1890). This reading — the
 *   exogenous_override_reading — holds that the Manifesto was capitulation to
 *   state power, not revelation; that the theological doctrine of plural
 *   marriage as an eternal covenant remains intact; and that only the
 *   practice was suspended under duress. The constraint is the standing
 *   arrangement: the Church's official position that the Manifesto was
 *   inspired counsel, which extracts ongoing doctrinal compliance from
 *   members who recognize the historical coercion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, 0.75).
domain_priors:suppression_score(marriage_commitment_legitimacy__exogenous_override_reading, 0.85).
domain_priors:theater_ratio(marriage_commitment_legitimacy__exogenous_override_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_legitimacy__exogenous_override_reading, "1890 Manifesto as Federal Coercion Forcing LDS Capitulation").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__exogenous_override_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__exogenous_override_reading, 'd6a9e8d7-f194-4964-81f3-4927617793de').
narrative_ontology:cs_kernel_codification('d6a9e8d7-f194-4964-81f3-4927617793de', formalized).
narrative_ontology:cs_authority_grounding('d6a9e8d7-f194-4964-81f3-4927617793de', lineage).
narrative_ontology:cs_interpretation_layer_present('d6a9e8d7-f194-4964-81f3-4927617793de').
narrative_ontology:cs_reading_relation('d6a9e8d7-f194-4964-81f3-4927617793de', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('d6a9e8d7-f194-4964-81f3-4927617793de', marriage_commitment_legitimacy__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('d6a9e8d7-f194-4964-81f3-4927617793de', foundational, manifesto_as_coerced_capitulation).
narrative_ontology:cs_axiom_status(manifesto_as_coerced_capitulation, holdable).
narrative_ontology:cs_axiom_grounding('d6a9e8d7-f194-4964-81f3-4927617793de', manifesto_as_coerced_capitulation, empirically_contingent).
narrative_ontology:cs_axiom('d6a9e8d7-f194-4964-81f3-4927617793de', foundational, plural_marriage_doctrine_unchanged).
narrative_ontology:cs_axiom_status(plural_marriage_doctrine_unchanged, holdable).
narrative_ontology:cs_axiom_grounding('d6a9e8d7-f194-4964-81f3-4927617793de', plural_marriage_doctrine_unchanged, deontological).
narrative_ontology:cs_reference_frame('d6a9e8d7-f194-4964-81f3-4927617793de', prophetic_independence_from_state_coercion).
narrative_ontology:cs_drift_state('d6a9e8d7-f194-4964-81f3-4927617793de', correlation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d6a9e8d7-f194-4964-81f3-4927617793de', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_leadership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, women_in_plural_marriages).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__exogenous_override_reading, federal_supremacy_over_territorial_governance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted and enforced the Edmunds Act (1882) and Edmunds-Tucker Act (1887) to dismantle the Church's corporate existence, seize property, disfranchise voters, and imprison leadership. The Manifesto extracted compliance: the Church formally abandoned plural marriage, enabling Utah statehood and federal control over territorial governance. The federal government gained sovereignty assertion and institutional subordination without negotiated settlement.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__exogenous_override_reading, federal_government, beneficiary).

% Issued the 1890 Manifesto under threat of institutional destruction: property seizure, disincorporation, leadership imprisonment, and loss of temples. The First Presidency framed the Manifesto as inspired counsel while privately acknowledging coercion (Woodruff's journal, Cannon's diary). They bear the cost of doctrinal surrender and the ongoing legitimacy management of presenting coercion as revelation. Exit would have meant institutional annihilation.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_leadership, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_leadership, payer).

% Bore the doctrinal and psychosocial costs: plural marriage families disrupted, theological framework ruptured (doctrine declared eternal then suspended), covenant obligations violated under duress. Members were not consulted; the Manifesto was presented as prophetic word. Identity lock operates through covenant theology, temple sealings, and communal narrative — exit means abandoning the salvation framework that defines selfhood. Some fled to Mexico/Canada; most stayed and internalized the rupture.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership, payer,
    organized, biographical, identity_locked, global).

% Continued plural marriage after 1890 (with leadership tacit approval until 1904 Second Manifesto). Rendered schismatic when the Church enforced the Manifesto retrospectively. Their voices — that the Manifesto was political not revelatory — were purged from institutional memory. Now constitute the fundamentalist movement, structurally excluded from mainstream LDS legitimacy.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, post_manifesto_plural_practitioners, excluded,
    moderate, biographical, trapped, regional).

% Experienced the most direct material and status disruption: legal vulnerability of marriages, uncertain inheritance, children's legitimacy questioned, social standing collapsed. Their testimony was not sought in the Manifesto process. The Church's later narratives erased their agency, framing plural marriage as solely male-privilege — a retrospective extraction that compounds the original coercion.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, women_in_plural_marriages, payer,
    powerless, biographical, constrained, local).

% Assess the Manifesto through documentary evidence (federal legislative record, Church leadership diaries, contemporary press, demographic data). Non-LDS historians (Gordon, Flake, Hardy) converge on coercion as primary driver; LDS institutional histories maintain revelatory framing. The analytical seat sees the structural extraction but cannot adjudicate the theological claim.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserved the Church's legal existence and property by ending the practice that triggered federal enforcement machinery — the Manifesto functioned as a capitulation document that stopped the Edmunds-Tucker dissolution.
% TRANSFER_FUNCTION: Moves institutional autonomy and doctrinal integrity from the LDS Church to the federal government, as the price of the Church's legal survival and leadership's physical freedom.
% ABSENT_VOICES: Plural marriage practitioners who continued the practice (fundamentalists), women in plural marriages whose status was rendered ambiguous, rank-and-file members who experienced the doctrinal rupture without consultation — all structurally excluded from the Manifesto's issuance and its subsequent institutional framing.
% DISAPPEARANCE_RATIONALE: The Manifesto is the linchpin of the Church's legal accommodation with the federal government; its removal would reactivate the coercive apparatus that forced it, resurrecting the federal campaign against Church corporate existence and reigniting the theological crisis over prophetic authority.
% FOUNDING_PROBLEM: Federal legislative and judicial campaign (Edmunds Act 1882, Edmunds-Tucker Act 1887) threatening the Church's corporate existence, property, temples, and leadership with imprisonment and disfranchisement.
% FOUNDING_PROBLEM_CORROBORATION: Historical consensus outside LDS apologetics confirms the federal campaign was the proximate cause; non-LDS historians of the period (Sarah Barringer Gordon, Kathleen Flake, B. Carmon Hardy) document the coercive apparatus from federal archives. LDS institutional histories now acknowledge the pressure while maintaining revelatory framing — the coercion is admitted but reinterpreted.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is high (0.75) because the federal government extracted institutional surrender without conceding theological ground — the Church gave up its distinguishing practice for legal survival. Suppression is very high (0.85) because alternatives were destroyed by force: plural marriage was criminalized, leadership imprisoned, property seized, and post-Manifesto enforcement (Second Manifesto 1904, purge of dissenters) made continuation impossible. Theater ratio is substantial (0.6) because the Manifesto is presented as revelation while functioning as capitulation; the gap between framing and mechanism requires continuous performative maintenance (Correlation curriculum, temple recommend interviews, official histories). Accessibility collapse is high (0.8) — no legal or institutional space for plural marriage exists within the Church. Resistance is significant (0.7) — documented in leadership diaries, fundamentalist schisms, and persistent member cognitive dissonance.
 *
 * PERSPECTIVAL GAP:
 *   From the federal seat, the Manifesto is a successful enforcement action — coordination of territorial governance. From the leadership seat, it is a tragic necessity — preservation of the institution at doctrinal cost. From the membership seat, it is an unexplained rupture — covenant theology violated by prophetic word. From the excluded seats, it is betrayal. The engine computes these divergences from the structural data; the authored claim (snare) reflects the analytical seat's reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government is the structural beneficiary (agenda_setter + beneficiary): it set the coercive terms and gained sovereignty assertion. LDS Church leadership is the primary target (agenda_setter + payer): they issued the Manifesto under duress and bear ongoing legitimacy costs. LDS membership is the deep victim (payer, identity_locked): they bear doctrinal rupture costs with no exit that preserves their salvation framework. Post-Manifesto practitioners and plural wives are excluded/payer seats whose voices were structurally silenced. The analytical seat sees the extraction but cannot resolve the theological claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal destruction of the Church) is dead — Utah statehood (1896), end of territorial status, and federal accommodation resolved the existential threat. Yet the constraint (the Manifesto as binding revelation) persists and has intensified (Second Manifesto 1904, ongoing excommunication for plural marriage advocacy). This is mandatrophy: the arrangement's original justification has expired but the constraint extracts ongoing compliance. The Church cannot acknowledge the founding problem's death without collapsing the revelatory framing that legitimizes the Manifesto.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the marriage_commitment_legitimacy kernel, or does it collapse into the hybrid_pragmatic_reading under scrutiny?',
    'Compare the exogenous_override_reading''s claim (doctrine unchanged, only practice suspended) against the hybrid_reading''s claim (strategic adaptation preserving core commitments through scope ambiguity). If the distinction is merely rhetorical — both acknowledge coercion and doctrinal continuity — the readings may not be structurally distinct constraints.',
    'If readings collapse, the kernel has fewer distinct constraints than declared; the exogenous_override_reading''s high extractiveness claim would need re-evaluation against the hybrid''s more nuanced extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the exogenous_override_reading is a structurally distinct constraint from its kernel siblings.').

omega_variable(
    coercion_vs_revelation_boundary,
    'Where does the historical coercion end and the theological framing begin? The leadership''s private acknowledgment of coercion (Woodruff, Cannon diaries) versus public revelatory framing creates a zone of ambiguity.',
    'Documentary analysis of leadership deliberations (1889-1890), the Manifesto''s textual evolution, and the 1891-1904 period of continued plural marriages with leadership knowledge. If leadership privately treated the Manifesto as tactical while publicly declaring it revelatory, the extraction is deliberate theater.',
    'Deliberate theater raises theater_ratio and confirms snare classification; genuine prophetic ambiguity would lower extractiveness and support tangled_rope or scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_vs_revelation_boundary, empirical, 'Whether the Manifesto''s revelatory framing was known to be false by its issuers.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the membership''s compliance sustained by structural suppression (excommunication, temple denial) or by internalized suppression (covenant theology making dissent spiritually impossible)?',
    'Post-exit trajectory study: members who leave the Church over this issue — does the suppression persist (internalized) or dissolve (structural)? Comparative analysis with other high-demand religious groups facing doctrinal reversals.',
    'If internalized, effective suppression is higher than structural measures suggest; the constraint extracts compliance even after formal exit. This would amplify the snare classification for the membership seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism for the identity-locked membership seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__exogenous_override_reading, 0, 134).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcl_exog_override_tr_t0, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(mcl_exog_override_tr_t14, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 14, 0.5).
narrative_ontology:measurement(mcl_exog_override_tr_t24, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 24, 0.55).
narrative_ontology:measurement(mcl_exog_override_tr_t44, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 44, 0.6).
narrative_ontology:measurement(mcl_exog_override_tr_t84, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 84, 0.6).
narrative_ontology:measurement(mcl_exog_override_tr_t134, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 134, 0.6).

% Extraction over time
narrative_ontology:measurement(mcl_exog_override_be_t0, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(mcl_exog_override_be_t14, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 14, 0.7).
narrative_ontology:measurement(mcl_exog_override_be_t24, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 24, 0.73).
narrative_ontology:measurement(mcl_exog_override_be_t44, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 44, 0.75).
narrative_ontology:measurement(mcl_exog_override_be_t84, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 84, 0.75).
narrative_ontology:measurement(mcl_exog_override_be_t134, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 134, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(mcl_exog_override_su_t0, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(mcl_exog_override_su_t14, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 14, 0.85).
narrative_ontology:measurement(mcl_exog_override_su_t24, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 24, 0.85).
narrative_ontology:measurement(mcl_exog_override_su_t44, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 44, 0.85).
narrative_ontology:measurement(mcl_exog_override_su_t84, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 84, 0.85).
narrative_ontology:measurement(mcl_exog_override_su_t134, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 134, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, utah_statehood_constitutional_constraint).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, lds_correlation_movement).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, second_manifesto_enforcement).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__exogenous_override_reading, fundamentalist_schism_formation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage_commitment_legitimacy kernel. The endogenous_reinterpretation_reading claims the Manifesto was revelation (low extractiveness, mountain-claimed). The hybrid_pragmatic_reading claims strategic adaptation (moderate extractiveness, tangled_rope). This reading claims coercion (high extractiveness, snare). The three readings share the same historical referent but author different ε values — they are distinct constraints linked by kernel membership.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_legitimacy__exogenous_override_reading, institutional, 0.2).
constraint_indexing:directionality_override(marriage_commitment_legitimacy__exogenous_override_reading, organized, 0.85).
constraint_indexing:directionality_override(marriage_commitment_legitimacy__exogenous_override_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
