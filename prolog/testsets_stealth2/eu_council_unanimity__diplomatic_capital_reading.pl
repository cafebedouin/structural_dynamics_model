% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__diplomatic_capital_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__diplomatic_capital_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: eu_council_unanimity__diplomatic_capital_reading
 *   human_readable: Council Unanimity as Consensus-Building Requirement (Diplomatic Capital Reading)
 *   domain: political/international_relations
 *
 * SUMMARY:
 *   Time anchoring: interval unit 0 = 2009 (Lisbon Treaty in force, qualified
 *   majority voting becomes default for ordinary legislation, leaving
 *   unanimity concentrated in foreign policy, taxation, treaty amendment, and
 *   designated sensitive files); unit 16 = 2025. This story instantiates ONE
 *   reading of the eu_council_unanimity kernel: the
 *   diplomatic_capital_reading, under which the unanimity requirement is a
 *   coordination cost purchased for a legitimacy payoff — iterative
 *   negotiation forces every government into ownership of the outcome, and
 *   unanimous acts prove more durable than qualified-majority impositions.
 *   Assessed by this reading's own lights, the standing arrangement extracts
 *   little (epsilon 0.23): the visible costs are deliberation time and
 *   concession flows that function as the price mechanism of consent, not as
 *   captured rent. CONSTRAINT FAMILY DECOMPOSITION (epsilon-invariance): the
 *   colloquial label 'Council unanimity' covers at least three structurally
 *   distinct claims. This reading prices low epsilon with no fixed victim
 *   structure; the sovereignty_guarantor_reading prices near-zero extraction
 *   from its own seat (the rule as protective wall against majoritarian
 *   coercion); the veto_trap_reading prices high epsilon with fixed payers
 *   (states and policy areas held hostage by credible blocking). Same treaty
 *   text, different referent assessments, different beneficiary structures,
 *   different failure modes — hence separate stories linked by network edges.
 *   KEY AGENTS (by structural relationship): - smaller_member_states: Core
 *   beneficiary (moderate/constrained) — formal veto parity converts
 *   small-state weight into courted consent - larger_member_states:
 *   Beneficiary with payer exposure (powerful/constrained) — supply most
 *   concessions and patience, collect continent-scale durable policy -
 *   eu_commission: Dual-positioned initiator (institutional/constrained) —
 *   pays negotiation delay on unanimous files, collects implementation
 *   reliability - rotating_council_presidencies: Process administrator
 *   (organized/mobile) — chairs and brokers consensus, accrues brokerage
 *   credit - national_parliaments_and_publics: Excluded seat
 *   (moderate/constrained) — receive executive-negotiated outputs without
 *   voice in the room - eu_integration_analysts: Analytical observer
 *   (analytical/analytical) — measure whether the durability premium is
 *   empirically real
 *
 * KEY AGENTS:
 *   - - smaller_member_states: Core beneficiary (moderate/constrained) — formal veto parity converts small-state weight into courted consent and concession income
 *   - - larger_member_states: Beneficiary with payer exposure (powerful/constrained) — bear disproportionate concession costs, collect durable continent-scale policy
 *   - - eu_commission: Dual-positioned initiator (institutional/constrained) — absorbs delay on unanimous files, gains implementation reliability and legitimacy stamping
 *   - - rotating_council_presidencies: Process administrator (organized/mobile) — runs the consensus machinery, collects brokerage prestige per rotation
 *   - - national_parliaments_and_publics: Excluded seat (moderate/constrained) — bound by executive bargains concluded without them
 *   - - eu_integration_analysts: Analytical observer (analytical/analytical) — adjudicate the durability-premium evidence from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__diplomatic_capital_reading, 0.23).
domain_priors:suppression_score(eu_council_unanimity__diplomatic_capital_reading, 0.14).
domain_priors:theater_ratio(eu_council_unanimity__diplomatic_capital_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, extractiveness, 0.23).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0.14).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__diplomatic_capital_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__diplomatic_capital_reading, "Council Unanimity as Consensus-Building Requirement (Diplomatic Capital Reading)").
narrative_ontology:topic_domain(eu_council_unanimity__diplomatic_capital_reading, "political/international_relations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__diplomatic_capital_reading, '52739653-435b-487e-aca8-e4c056079ec5').
narrative_ontology:cs_kernel_codification('52739653-435b-487e-aca8-e4c056079ec5', formalized).
narrative_ontology:cs_authority_grounding('52739653-435b-487e-aca8-e4c056079ec5', lineage).
narrative_ontology:cs_interpretation_layer_present('52739653-435b-487e-aca8-e4c056079ec5').
narrative_ontology:cs_reading_relation('52739653-435b-487e-aca8-e4c056079ec5', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('52739653-435b-487e-aca8-e4c056079ec5', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_axiom('52739653-435b-487e-aca8-e4c056079ec5', foundational, consent_purchase_yields_durability_premium).
narrative_ontology:cs_axiom_status(consent_purchase_yields_durability_premium, holdable).
narrative_ontology:cs_axiom_grounding('52739653-435b-487e-aca8-e4c056079ec5', consent_purchase_yields_durability_premium, empirically_contingent).
narrative_ontology:cs_axiom('52739653-435b-487e-aca8-e4c056079ec5', foundational, universal_assent_confers_legitimacy_surplus).
narrative_ontology:cs_axiom_status(universal_assent_confers_legitimacy_surplus, holdable).
narrative_ontology:cs_axiom_grounding('52739653-435b-487e-aca8-e4c056079ec5', universal_assent_confers_legitimacy_surplus, deontological).
narrative_ontology:cs_reference_frame('52739653-435b-487e-aca8-e4c056079ec5', unanimous_assent_as_legitimacy_source).
narrative_ontology:cs_drift_state('52739653-435b-487e-aca8-e4c056079ec5', contemporary_post_enlargement_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('52739653-435b-487e-aca8-e4c056079ec5', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, smaller_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, larger_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, eu_commission).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(eu_council_unanimity__diplomatic_capital_reading, larger_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__diplomatic_capital_reading, eu_commission).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Governments of the Union's smallest members hold formally identical blocking power to the largest. Because every file governed by unanimity must come to them, their assent is courted with concessions, opt-outs, and package sweeteners, converting a small economy's actual weight into outsized negotiating leverage. They cannot shed the unanimity rule for foreign policy, taxation, or treaty amendment short of treaty change or leaving the Union; within the room their consent is the asset the rule makes valuable.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, smaller_member_states, beneficiary,
    moderate, biographical, constrained, continental).

% The Germany- and France-class governments supply most of the financing, concessions, and patience that assembling twenty-seven consents requires; each major initiative costs them disproportionate bargaining effort and side-payments. In exchange they obtain continent-scale instruments — sanctions regimes, recovery programs, treaty change — that bind every member and survive electoral turnover in a way a bare majority imposition on a contested file would not. They cannot unilaterally convert the remaining unanimous files to voting, and exiting would forfeit the scale benefits.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, larger_member_states, beneficiary,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__diplomatic_capital_reading, larger_member_states, payer).

% Initiates most Union policy and implements whatever passes. On files still governed by unanimity it must invest months brokering consensus instead of drafting for a qualified majority, and it absorbs the delay whenever a single government holds out. What it collects in return is implementation reliability: unanimous acts arrive with every government already publicly signed on, transposition conflicts are rarer, and flagship instruments carry a legitimacy stamp that quiet majority votes lack. Its agenda-setting freedom is bounded by which legal bases remain unanimous.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_commission, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__diplomatic_capital_reading, eu_commission, payer).

% Each six-month presidency chairs the Council formations, drafts compromise texts, and shuttles between capitals to assemble the required consents. It administers the consensus process without owning the rule — the treaties fix unanimity; the presidency works within it, using silence procedures and package construction. The rotation hands each government a recurring turn accumulating brokerage credit, mediation experience, and visibility that outlasts its term.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, rotating_council_presidencies, agenda_setter,
    organized, immediate, mobile, continental).

% National legislatures and electorates receive consensual outputs negotiated by their executives behind closed doors. They are presented with a position described as agreed unanimously and asked to ratify or live with it after the bargaining is finished; the permissive-consensus pattern lets governments speak for publics who were never in the room. Their recourse runs through domestic elections and coalition formation, not through the Council process itself.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, national_parliaments_and_publics, excluded,
    moderate, biographical, constrained, national).

% Comparative-politics and EU-studies researchers track whether unanimous instruments actually deliver the durability premium the consensus account promises — comparing transposition lags, infringement rates, and amendment frequency of unanimous versus qualified-majority acts. They publish the evidence both camps cite and hold no stake in any outcome.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_integration_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__diplomatic_capital_reading, diffuse).
narrative_ontology:fixing_cost_class(eu_council_unanimity__diplomatic_capital_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of committing twenty-seven sovereign states to common action that each must then implement: requiring every state's assent produces decisions every state owns, suppressing post-adoption sabotage and enabling joint commitments credible enough to outlast electoral turnover in any single capital.
% TRANSFER_FUNCTION: Moves concessions, budget adjustments, and legal exemptions from the states most eager for collective action (typically the largest) toward whichever governments' assent must be assembled; moves months of negotiating time and ministerial attention from all twenty-seven into iterative package bargaining; and returns to all participants a binding instrument each has publicly endorsed.
% ABSENT_VOICES: National parliaments and publics are outside the Council room and would object that consensus is executive bargains presented as faits accomplis ('permissive consensus'); affected third parties — neighboring states, accession candidates bound by unanimous external action — also have no seat. They surface domestically only after agreements are closed.
% DISAPPEARANCE_RATIONALE: If unanimity vanished overnight and qualified majority voting covered all files, larger-state coalitions would legislate past laggards immediately, blocked dossiers (tax harmonization, sanction renewals, treaty change) would move, and the consent market in which every government trades its assent would evaporate — smaller states would lose their parity lever, implementation cooperation on contested files would degrade, and the treaty-amendment lock would dissolve into ordinary majority politics.
% FOUNDING_PROBLEM: From the empty-chair crisis of 1965 through the Luxembourg compromise and into the CFSP design, member states demanded that no government ever be bound by collective action it had not consented to; unanimity was constructed so that joint action proceeds only by assent, making every act self-legitimating for each participating state.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the direct beneficiary set: intergovernmentalist integration scholarship documenting compliance and transposition differentials between unanimous and qualified-majority instruments; published testimony of former foreign ministers and Council secretaries-general on why capitals insisted on assent; and candidate-country assessments of how Union commitments are made. The corroboration is real but contested — efficiency-oriented analysts and the Commission's own QMV-extension proposals attest blockage costs this reading discounts.
narrative_ontology:disappearance_verdict(eu_council_unanimity__diplomatic_capital_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__diplomatic_capital_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__diplomatic_capital_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eu_council_unanimity__diplomatic_capital_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__diplomatic_capital_reading, 0.23, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__diplomatic_capital_reading_tests).
:- end_tests(eu_council_unanimity__diplomatic_capital_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored low (0.23) because this reading assesses the standing unanimity arrangement as consent-purchase whose costs are priced exchange, with the referent fixed as the existing arrangement (never the QMV regime this reading declines to endorse). Suppression (0.14) is authored as a RAW structural property — unscaled by power or scope; only extractiveness is scaled by the engine — reflecting the mild social cost of lone-blockade isolation rather than coercive enforcement; no suppression_requirement series is authored because the enforcement picture is static: unanimity needs no enforcement machinery, it is self-executing procedure. Theater ratio is low (0.12) — the negotiation function is real and load-bearing; the modest drift upward tracks summit-communique unity performance during crises. Accessibility collapse (0.40) reflects that the main alternative (qualified majority) is well understood and already governs most legislation, so the remaining unanimity persists alongside a live, demonstrated substitute. Resistance (0.38) reflects sustained QMV-extension pressure from the Parliament, Commission proposals, and larger-state advocacy. The measurement series run on ONE shared time grid ({0,3,6,9,12,16}) with both tracked metrics authored at every point; the gentle upward epsilon drift models rising deliberation burden at twenty-seven members — which this reading books as coordination cost, not accumulation of extraction.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply over the identical rule text. From the practitioner seat (governments, presidency brokers), the arrangement is coordination they actively use: each government banks diplomatic capital, and the durability of unanimous instruments is experienced directly in smoother transposition. From a federalist efficiency-critic seat, the same structure computes as enforced delay bordering on extraction — which is the veto_trap sibling's constraint, not this one. From a sovereignty-guardian seat, the rule is a protective wall whose 'cost' framing misses the point entirely. This file authors only the diplomatic-capital seat's constraint with its own stable epsilon; the engine computes per-seat classifications from the structural data, and divergence between this claim and other seats' computations is the intended measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   All three declared beneficiaries derive directionality toward the subsidy end: smaller states (damped strongly — the rule subsidizes their weight), larger states (damped less; their secondary payer exposure lifts them toward symmetric, since they finance most concessions), and the Commission (mid-range: pays delay, collects reliability). No victims are declared because this reading identifies no group that systematically bears uncompensated extraction — concession flows are the visible half of a consent exchange both sides price. Excluded seats (parliaments, publics) sit outside the consent market and outside the derivation; the presidency administers the process and collects incidental brokerage credit rather than constraint-derived benefit, so it is not listed as beneficiary. Larger spatial scope raises verification difficulty, which the engine applies to effective extraction — modestly inflating an already-low base.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim disciplines both sibling errors. Against the veto_trap reading, it insists the coordination function is genuine and measurable — compliance and transposition differentials favor unanimous instruments — so classifying unanimity as pure extraction would erase a real buy-in mechanism. Against a sovereignty-reading drift toward naturalization, the accessibility and resistance metrics insist unanimity is a chosen institutional rule with a demonstrated, functioning alternative, not an unchangeable limit — it has been narrowed by treaty revision twice. Mandatrophy is not resolved: the founding problem (binding sovereigns without coercion) remains live at twenty-seven members, and the arrangement's persistence tracks continued utility rather than atrophied inertia. The receipt-surface shape (diffuse gains, prohibitive fixing) is noted deliberately: it is the cost-asymmetry signature of an entrenched-but-functioning institution — replacement requires unanimous treaty change ratified twenty-seven times over, which no sitting coalition has been willing to fund — and it is distinguished from piton by low theater (0.12) and a live, load-bearing coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location_disagreement,
    'This story instantiates only the diplomatic_capital_reading of the eu_council_unanimity kernel; the sibling readings (sovereignty_guarantor_reading, veto_trap_reading) instantiate different constraints over the identical rule text — at which structural element do the three readings actually disagree?',
    'Side-by-side compilation and classification of all three reading stories: determine whether the divergence sits in the epsilon referent assessment, in the beneficiary/victim structure (positional versus fixed), or in the persistence mechanism (consent-purchase, constitutional guarantee, or coercive holdup).',
    'If the disagreement reduces to epsilon indexing alone, the three are one constraint seen from three seats and the network edges overstate separation; if beneficiary structure differs as authored, they are three distinct constraints and the family edges carry a genuine structural contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location_disagreement, conceptual, 'Committer-frame omega locating the structural axis on which the three unanimity readings separate.').

omega_variable(
    durability_premium_empirical_warrant,
    'Does the durability premium this reading prices into its low epsilon — fewer transposition failures, longer instrument life, higher compliance for unanimous acts — survive at twenty-seven members and on politicized files?',
    'Matched comparison of unanimous versus qualified-majority instruments on transposition lag, infringement rates, and amendment frequency, controlling for policy domain and political salience.',
    'If the premium vanishes or reverses, this reading''s epsilon is understated and its foundational empirical axiom loses warrant, pushing computed classifications toward the veto_trap sibling''s territory; if it holds, the authored low epsilon is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(durability_premium_empirical_warrant, empirical, 'Empirical test of the consent-durability linkage underlying the reading''s low extraction assessment.').

omega_variable(
    alternative_cs_framing_practice_kernel,
    'Is the kernel better framed as the formalized treaty rule (formalized codification, lineage authority) or as the accumulated practice of consensus-seeking (implicit kernel, practitioner-custom authority), and does the choice change the commitment-system classification?',
    'Trace whether the operative softening devices — constructive abstention, stopping-the-clock, silence procedures — originate in textual interpretation or in uncodified practitioner custom; if practice carries the operative rule, re-author with kernel_codification implicit and authority_grounding practice.',
    'Under the practice framing, the interpretation layer becomes the whole system rather than a buffer beneath a formal kernel, shifting drift diagnosis toward practice_drift as the primary vector; the constraint type itself is unlikely to move.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_cs_framing_practice_kernel, conceptual, 'CS-framing under-determination: formalized-text kernel versus practice-carried kernel for the same unanimity arrangement.').

omega_variable(
    legitimacy_transfer_measurement,
    'How much legitimacy does unanimous adoption actually transfer to an instrument in public and parliamentary perception, independent of elite assertion?',
    'Eurobarometer tracking and parliamentary-debate analysis contrasting support trajectories of unanimous versus majority-adopted instruments of comparable salience.',
    'If measured legitimacy transfer is negligible, the coordination benefit shrinks toward pure delay and the reading''s net-benefit premise weakens; if material, the rope classification holds with the authored epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_transfer_measurement, empirical, 'Whether the legitimacy payoff the reading claims is perceptible outside the negotiating elite.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__diplomatic_capital_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(eu_c_tr_t3, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 3, 0.09).
narrative_ontology:measurement(eu_c_tr_t6, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(eu_c_tr_t9, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 9, 0.11).
narrative_ontology:measurement(eu_c_tr_t12, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 12, 0.11).
narrative_ontology:measurement(eu_c_tr_t16, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 16, 0.12).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(eu_c_be_t3, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 3, 0.21).
narrative_ontology:measurement(eu_c_be_t6, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 6, 0.21).
narrative_ontology:measurement(eu_c_be_t9, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 9, 0.22).
narrative_ontology:measurement(eu_c_be_t12, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 12, 0.22).
narrative_ontology:measurement(eu_c_be_t16, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 16, 0.23).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(eu_council_unanimity__diplomatic_capital_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__diplomatic_capital_reading, resource_allocation).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__veto_trap_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Council unanimity' decomposes into at least three structurally distinct constraints sharing one treaty text. This diplomatic-capital reading authors low epsilon (0.23) with no fixed victim structure — costs are priced consent exchange. The sovereignty_guarantor_reading authors near-zero extraction from its own seat (the rule as protective wall). The veto_trap_reading authors high epsilon with fixed payers (held-hostage policy areas and coalitions-of-the-willing forced into side-payments). Epsilon differs because the referent assessment is reading-indexed over a shared arrangement, not because the observables vary within any one story. Evidential traffic runs both directions along the edges: unanimous-action track records (sanctions packages, recovery instruments) feed this reading's warrant; high-profile blocking episodes feed the trap reading; constitutional moments feed the sovereignty reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
