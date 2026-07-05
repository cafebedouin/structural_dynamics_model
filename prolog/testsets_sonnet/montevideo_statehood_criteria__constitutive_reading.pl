% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__constitutive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__constitutive_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: montevideo_statehood_criteria__constitutive_reading
 *   human_readable: Constitutive Recognition Doctrine of Statehood
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the constitutive reading of the Montevideo
 *   statehood kernel: statehood is not simply a fact about a polity's
 *   territory, population, government, and capacity for relations, but a
 *   status conferred by the community of already-recognized states. Under
 *   this reading, an entity meeting every objective Montevideo criterion
 *   remains, in the fullest legal sense, not-a-state until enough of the
 *   existing community chooses to treat it as one. This is a distinct
 *   constraint from the declaratory reading (where the objective criteria
 *   alone establish statehood as fact, independent of third-party will) and
 *   from the hybrid reading (where objective criteria plus normative
 *   legitimacy conditions jointly govern statehood). Each reading has a
 *   different ε, a different beneficiary/victim structure, and a different
 *   classification, and each is authored as its own constraint story per the
 *   ε-invariance principle.
 *
 * KEY AGENTS:
 *   - existing_recognized_states: agenda_setter (institutional/arbitrage) — control admission
 *   - un_security_council_permanent_members: beneficiary/agenda_setter (institutional/arbitrage) — veto UN membership
 *   - unrecognized_polities: payer (powerless/trapped) — meet criteria, denied standing
 *   - de_facto_states: payer (powerless/constrained) — functioning but excluded for decades
 *   - populations_of_contested_territories: payer (powerless/trapped) — bear humanitarian cost
 *   - patron_states: beneficiary (powerful/mobile) — leverage selective recognition strategically
 *   - international_legal_scholars: observer (analytical/analytical) — document the doctrine's operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, 0.68).
domain_priors:suppression_score(montevideo_statehood_criteria__constitutive_reading, 0.71).
domain_priors:theater_ratio(montevideo_statehood_criteria__constitutive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__constitutive_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__constitutive_reading, "Constitutive Recognition Doctrine of Statehood").
narrative_ontology:topic_domain(montevideo_statehood_criteria__constitutive_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__constitutive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__constitutive_reading, '70f23e04-280b-4220-88d4-d61a55f18893').
narrative_ontology:cs_kernel_codification('70f23e04-280b-4220-88d4-d61a55f18893', formalized).
narrative_ontology:cs_authority_grounding('70f23e04-280b-4220-88d4-d61a55f18893', distributed).
narrative_ontology:cs_reading_relation('70f23e04-280b-4220-88d4-d61a55f18893', montevideo_statehood_criteria__declaratory_reading, forecloses).
narrative_ontology:cs_reading_relation('70f23e04-280b-4220-88d4-d61a55f18893', montevideo_statehood_criteria__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('70f23e04-280b-4220-88d4-d61a55f18893', foundational, recognition_is_constitutive_not_evidentiary).
narrative_ontology:cs_axiom_status(recognition_is_constitutive_not_evidentiary, holdable).
narrative_ontology:cs_axiom_grounding('70f23e04-280b-4220-88d4-d61a55f18893', recognition_is_constitutive_not_evidentiary, conventional).
narrative_ontology:cs_axiom('70f23e04-280b-4220-88d4-d61a55f18893', secondary, existing_states_hold_legitimate_veto_over_new_state_admission).
narrative_ontology:cs_axiom_status(existing_states_hold_legitimate_veto_over_new_state_admission, holdable).
narrative_ontology:cs_axiom_grounding('70f23e04-280b-4220-88d4-d61a55f18893', existing_states_hold_legitimate_veto_over_new_state_admission, conventional).
narrative_ontology:cs_reference_frame('70f23e04-280b-4220-88d4-d61a55f18893', post_westphalian_state_consent_order).
narrative_ontology:cs_drift_state('70f23e04-280b-4220-88d4-d61a55f18893', post_decolonization_and_secession_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('70f23e04-280b-4220-88d4-d61a55f18893', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, existing_recognized_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, un_security_council_permanent_members).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, de_facto_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, populations_of_contested_territories).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, patron_states).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__constitutive_reading, sovereign_equality_of_recognized_states).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__constitutive_reading, state_consent_as_source_of_international_legal_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collectively hold the gatekeeping power to admit or refuse new entrants into the interstate system. Grant or withhold bilateral recognition, vote on UN membership, and set the informal threshold of how many recognitions constitute 'enough.' Bear essentially no cost from withholding recognition and often gain leverage (border concessions, resource deals, diplomatic loyalty) by making recognition conditional.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, existing_recognized_states, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Hold veto power over UN membership, which is the highest-value form of collective recognition. Use recognition and non-recognition of contested entities as instruments of geopolitical strategy (e.g., competing recognitions of breakaway regions), converting a legal doctrine into a bargaining chip in unrelated disputes.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, un_security_council_permanent_members, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__constitutive_reading, un_security_council_permanent_members, agenda_setter).

% Function as de facto governments controlling territory and population, meeting the objective Montevideo criteria (permanent population, defined territory, government, capacity for relations), but are denied treaty-making capacity, IMF/World Bank access, ICJ standing, and international banking access because the required recognitions never accumulate. Their exit options are limited to seeking patron-state recognition, which subordinates them to a new dependency.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities, payer,
    powerless, generational, trapped, regional).

% Operate functioning institutions, currencies, and security forces for decades without admission to the interstate system. Cannot access international courts to resolve disputes with recognized neighbors, cannot borrow on sovereign terms, and are excluded from climate, trade, and health treaty regimes that materially affect their populations.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, de_facto_states, payer,
    powerless, generational, constrained, regional).

% Live under governments whose international legal capacity is contingent on third-party recognition they did not choose and cannot obtain through their own conduct. Lack passports usable for travel, cannot access international dispute-resolution mechanisms, and bear the humanitarian costs of a legal status frozen by other states' strategic calculations.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, populations_of_contested_territories, payer,
    powerless, biographical, trapped, local).

% Extend selective recognition to breakaway or contested entities as a lever against rival states, gaining a client polity and strategic depth without bearing the costs of formal annexation. Their recognition decisions are treated as partially constitutive, giving them outsized influence over which unrecognized polities gain any international standing at all.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, patron_states, beneficiary,
    powerful, generational, mobile, regional).

% Study and debate whether recognition creates or merely acknowledges statehood, producing the doctrinal literature that both justifies and critiques the constitutive position. Their scholarship is cited by all sides but does not itself alter which entities are recognized.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the community of states with a mechanism to control admission into treaty regimes, international organizations, and the mutual-recognition system that underwrites diplomatic and economic relations — preventing every territorial claimant from unilaterally asserting equal standing.
% TRANSFER_FUNCTION: Moves legal capacity, treaty access, international credit, and dispute-resolution standing from polities that meet objective statehood criteria but lack sufficient recognition, to the discretion of already-recognized states, who can withhold or grant that capacity as a strategic instrument.
% ABSENT_VOICES: Unrecognized polities and de facto states have no vote in the recognition decisions that determine their own legal existence; they can lobby individual states bilaterally but have no forum in which their claim to statehood is adjudicated by criteria rather than by the political interests of recognizers.
% DISAPPEARANCE_RATIONALE: If constitutive recognition doctrine were abandoned overnight in favor of pure declaratory statehood, dozens of de facto states and contested territories would acquire treaty capacity, UN standing eligibility, and international credit access without further political negotiation — reorganizing membership in nearly every international organization and forcing renegotiation of disputed borders and resource claims currently frozen by non-recognition.
% FOUNDING_PROBLEM: In the early-to-mid twentieth century, the interstate system needed a way to prevent the proliferation of unstable, non-viable, or puppet entities from claiming full international legal personality and treaty rights, and to preserve orderly control over admission to the community of nations following decolonization and state fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: Existing states and their foreign ministries attest the doctrine remains necessary to prevent destabilizing unilateral secession and to preserve orderly international order. Independent international law scholars (e.g., in academic commentary on Kosovo, Somaliland, and Taiwan) and human rights bodies documenting the effects of non-recognition on contested populations attest that the doctrine now functions primarily as a geopolitical veto rather than a genuine viability filter, since polities meeting every objective criterion remain excluded for decades on political grounds alone.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__constitutive_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__constitutive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__constitutive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__constitutive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__constitutive_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__constitutive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__constitutive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) reflects the substantial transfer of legal capacity, treaty access, and credit standing from polities that meet objective criteria to the discretion of already-recognized states. Suppression (0.71) is high because the doctrine's persistence depends on active diplomatic enforcement — states coordinate non-recognition, block UN admission votes, and threaten secondary consequences (sanctions, non-recognition of passports, exclusion from regional bodies) against would-be recognizers. Theater ratio (0.40) captures that a meaningful share of recognition practice increasingly serves geopolitical signaling (competitive recognition of breakaway regions as proxy conflict) rather than genuine assessment of governmental effectiveness or population welfare. Accessibility collapse (0.62) is moderate-high: once a polity is denied broad recognition, essentially all high-value international legal, financial, and diplomatic alternatives close off, though bilateral and patron-state workarounds partially persist. Resistance (0.58) reflects sustained pushback from unrecognized and de facto states through unilateral declarations, international law advocacy, and appeals to the ICJ (e.g., the Kosovo advisory opinion) — resistance that has produced doctrinal erosion but not abandonment of the constitutive practice.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of existing recognized states, constitutive recognition looks like orderly, prudent gatekeeping against destabilizing proliferation of unviable claimants — a genuine coordination function protecting the integrity of the treaty system. From the seat of a de facto state that has run functioning institutions for thirty years without admission, the same doctrine looks like naked political extraction: an arbitrary veto exercised by parties with no stake in the territory's governance, whose real function is preserving existing states' leverage rather than assessing viability. The engine's per-seat computation should reflect this asymmetry directly from the declared power/exit differentials, not from any narrative framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Existing recognized states and Security Council permanent members sit near the full-beneficiary end: they administer the recognition apparatus, incur no structural cost from withholding it, and can convert recognition into strategic leverage. Patron states occupy an intermediate beneficiary position — they gain client-polity relationships through selective recognition without bearing the doctrine's full institutional weight. Unrecognized polities, de facto states, and populations of contested territories sit at the full-target end: they meet the objective criteria this reading declines to treat as sufficient, and their trapped/constrained exit options (no alternative legal system to appeal to, no unilateral path to standing) amplify the effective extraction they bear.
 *
 * MANDATROPHY ANALYSIS:
 *   The constitutive reading's founding problem — preventing unstable or non-viable entities from claiming full international legal personality — was live at the doctrine's mid-twentieth-century consolidation, when decolonization produced genuine uncertainty about which emergent polities could sustain treaty obligations. That problem is now contested: many currently unrecognized entities (Somaliland, Taiwan, Kosovo pre-2008) have sustained functioning governance for decades, meeting every objective viability criterion the doctrine was meant to test. Classifying this as tangled_rope rather than snare preserves the genuine coordination function the doctrine still performs (screening against transparently non-viable secessionist claims, puppet states, or entities lacking any functioning government) while naming the asymmetric extraction that persists once viability is no longer genuinely in question — collapsing it to pure snare would erase the doctrine's residual coordination value; collapsing it to rope would launder the recognized states' veto power as costless.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutive_vs_declaratory_kernel_reading,
    'Is statehood constituted by recognition (this reading), or does statehood exist as a legal fact once the four objective Montevideo criteria are met regardless of recognition (declaratory_reading), or does it require criteria plus normative legitimacy conditions (hybrid_reading)?',
    'No empirical resolution is possible; this is a doctrinal/framing choice within international law with genuine disagreement among states, courts, and scholars. State practice is inconsistent (states invoke declaratory language for some entities and constitutive practice for others depending on strategic interest), and no single tribunal has final authority to settle the kernel dispute.',
    'Under the constitutive reading, unrecognized polities meeting objective criteria remain victims of a structural veto with no legal remedy. Under the declaratory reading, the same polities would already possess statehood as a legal fact and recognition would be merely evidentiary, dissolving most of the victim set this story authors. The hybrid reading would split the victim set further, excluding only those failing the normative legitimacy prong.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutive_vs_declaratory_kernel_reading, conceptual, 'Which kernel reading of Montevideo statehood is structurally correct is irreducibly contested; this story commits to the constitutive reading only.').

omega_variable(
    recognition_as_political_instrument_vs_genuine_screen,
    'Does the community-of-states recognition requirement still function as a genuine screen against non-viable claimants, or has it become primarily a geopolitical bargaining instrument decoupled from any assessment of governmental effectiveness?',
    'Comparative case analysis: examine recognition/non-recognition decisions against objective governance-effectiveness indicators (tax collection, monopoly on force, service delivery, population consent) across a sample of contested entities, to test whether recognition outcomes track viability or track the strategic interests of recognizing states.',
    'If recognition decisions track viability, the coordination function is substantially intact and the tangled_rope classification with a genuine coordination component is well-supported. If recognition decisions track only strategic interest independent of viability, the constitutive doctrine functions closer to pure extraction (snare) for the excluded polities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_as_political_instrument_vs_genuine_screen, empirical, 'Whether the coordination rationale for constitutive recognition is still operative or has been substantially displaced by strategic bargaining.').

omega_variable(
    veto_concentration_among_permanent_members,
    'Is the extraction better modeled as diffuse (all existing recognized states benefit equally from gatekeeping power) or concentrated (a small number of powerful states, particularly UNSC permanent members and regional hegemons, capture most of the leverage)?',
    'Analysis of which states'' recognition decisions are treated as practically decisive for UN membership and major treaty access versus which states'' recognition is largely symbolic.',
    'A concentrated-capture finding would support elevating UNSC permanent members to a more central beneficiary role and re-examining whether the classification should shift toward emphasizing capture by a narrow bloc rather than the community of states broadly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_concentration_among_permanent_members, empirical, 'Whether extraction capture is diffuse across recognizing states or concentrated among the most powerful.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__constitutive_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t0, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mont_tr_t15, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(mont_tr_t30, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(mont_tr_t45, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 45, 0.34).
narrative_ontology:measurement(mont_tr_t60, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 60, 0.37).
narrative_ontology:measurement(mont_tr_t75, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 75, 0.4).

% Extraction over time
narrative_ontology:measurement(mont_be_t0, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mont_be_t15, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(mont_be_t30, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement(mont_be_t45, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 45, 0.61).
narrative_ontology:measurement(mont_be_t60, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(mont_be_t75, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 75, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t0, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(mont_su_t15, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement(mont_su_t30, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 30, 0.61).
narrative_ontology:measurement(mont_su_t45, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 45, 0.65).
narrative_ontology:measurement(mont_su_t60, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(mont_su_t75, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 75, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__constitutive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the montevideo_statehood_criteria kernel: constitutive_reading (this story — recognition constitutes statehood, existing states hold structural veto), declaratory_reading (objective criteria alone establish statehood as legal fact independent of recognition), and hybrid_reading (objective criteria plus normative legitimacy conditions jointly govern statehood). Each reading has a distinct ε, victim set, and classification. The constitutive reading produces the largest victim set (any polity meeting objective criteria but lacking sufficient recognition) and the highest suppression (recognition withholding requires active diplomatic coordination); the declaratory reading would produce near-mountain metrics (criteria are objectively checkable, no third-party veto); the hybrid reading falls between, since normative legitimacy conditions reintroduce discretionary judgment but with a narrower veto scope than pure recognition-based constitution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
