% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__contextual_defensive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__contextual_defensive, []).

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
 *   constraint_id: quran_9_5_scope__contextual_defensive
 *   human_readable: Verse 9:5 Contextual-Defensive Reading (Treaty Priority, Defensive-War Limit)
 *   domain: religious/hermeneutic/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the quran_9_5_scope kernel: the
 *   contextual_defensive reading, under which verse 9:5 addresses the
 *   treaty-breaking polytheist tribes of seventh-century Medina, does not
 *   abrogate the peaceful verses, and licenses only defensive and
 *   treaty-enforcing force. As a standing arrangement, the reading functions
 *   as the interpretive-legal infrastructure by which Muslim-majority states
 *   sustain treaties, minority communities hold security guarantees, and the
 *   juridical establishment certifies the boundary of legitimate violence.
 *   The epsilon referent is the standing contextual-defensive arrangement
 *   itself, assessed by this reading's own lights, not the abrogating or
 *   progressive alternatives (those are separate constraint files). The
 *   claim/metric gap is deliberate and independent: the constraint is CLAIMED
 *   as rope (genuine coordination, minimal coercive overhead, net
 *   beneficiaries) while the metrics describe modest but real drift, and the
 *   engine measures whatever divergence exists.
 *
 * KEY AGENTS:
 *   - mainstream_juridical_establishment: agenda-setter (institutional/identity_locked) — administers the reading, collects authority-rents, bears the cost of defending it
 *   - integrationist_muslim_majority_states: primary beneficiary (institutional/constrained) — converts the reading into diplomatic standing and alliance maintenance
 *   - non_muslim_minority_communities: protected beneficiary (moderate/constrained) — security rides on the defensive-only limit
 *   - treaty_breaking_aggressors: sole payer seat (moderate/constrained) — bears the coercive edge only after its own breach
 *   - militant_rejectionist_movements: excluded voice (organized/identity_locked) — holds the rival abrogationist reading outside the certified process
 *   - devout_pluralist_believers: diffuse beneficiary (powerless/identity_locked) — inhabits the reading as lived piety
 *   - academic_quranic_studies: analytical observer — attests the historical-referent question from outside the beneficiary set
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__contextual_defensive, 0.18).
domain_priors:suppression_score(quran_9_5_scope__contextual_defensive, 0.28).
domain_priors:theater_ratio(quran_9_5_scope__contextual_defensive, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, extractiveness, 0.18).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__contextual_defensive, rope).
narrative_ontology:human_readable(quran_9_5_scope__contextual_defensive, "Verse 9:5 Contextual-Defensive Reading (Treaty Priority, Defensive-War Limit)").
narrative_ontology:topic_domain(quran_9_5_scope__contextual_defensive, "religious/hermeneutic/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__contextual_defensive, '2802ff29-9bbc-4a9d-a86c-77e148400bf9').
narrative_ontology:cs_kernel_codification('2802ff29-9bbc-4a9d-a86c-77e148400bf9', fixed_text).
narrative_ontology:cs_authority_grounding('2802ff29-9bbc-4a9d-a86c-77e148400bf9', lineage).
narrative_ontology:cs_interpretation_layer_present('2802ff29-9bbc-4a9d-a86c-77e148400bf9').
narrative_ontology:cs_reading_relation('2802ff29-9bbc-4a9d-a86c-77e148400bf9', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('2802ff29-9bbc-4a9d-a86c-77e148400bf9', quran_9_5_scope__progressive_synthesis, influences).
narrative_ontology:cs_axiom('2802ff29-9bbc-4a9d-a86c-77e148400bf9', foundational, peaceful_verses_remain_operative).
narrative_ontology:cs_axiom_status(peaceful_verses_remain_operative, holdable).
narrative_ontology:cs_axiom_grounding('2802ff29-9bbc-4a9d-a86c-77e148400bf9', peaceful_verses_remain_operative, theological).
narrative_ontology:cs_axiom('2802ff29-9bbc-4a9d-a86c-77e148400bf9', foundational, war_requires_prior_aggression_or_treaty_breach).
narrative_ontology:cs_axiom_status(war_requires_prior_aggression_or_treaty_breach, holdable).
narrative_ontology:cs_axiom_grounding('2802ff29-9bbc-4a9d-a86c-77e148400bf9', war_requires_prior_aggression_or_treaty_breach, deontological).
narrative_ontology:cs_reference_frame('2802ff29-9bbc-4a9d-a86c-77e148400bf9', medinan_covenant_defensive_baseline).
narrative_ontology:cs_drift_state('2802ff29-9bbc-4a9d-a86c-77e148400bf9', contemporary_global_media_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('2802ff29-9bbc-4a9d-a86c-77e148400bf9', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__contextual_defensive, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, non_muslim_minority_communities).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, mainstream_juridical_establishment).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, treaty_breaking_aggressors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, devout_pluralist_believers).
narrative_ontology:constraint_vindicates(quran_9_5_scope__contextual_defensive, treaty_inviolability_doctrine).
narrative_ontology:constraint_vindicates(quran_9_5_scope__contextual_defensive, defensive_warfare_principle).
narrative_ontology:constraint_vindicates(quran_9_5_scope__contextual_defensive, occasionalist_revelation_method).
narrative_ontology:constraint_vindicates(quran_9_5_scope__contextual_defensive, prophetic_covenant_precedent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Certifies and transmits the contextual-defensive reading through seminary curricula, fatwa bodies, and international declarations such as the Amman Message and the Marrakesh Declaration. Collects scholarly authority, endowment income, and state patronage from administering the reading, and spends that accumulated authority defending it against rival interpretive movements. Its standing is constituted by the transmission chain it administers, so abandoning the reading would dissolve the source of its own authority.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, mainstream_juridical_establishment, agenda_setter,
    institutional, generational, identity_locked, global).

% Invoke the reading in diplomacy, constitutional guarantees for non-Muslim citizens, and alliance maintenance with non-Muslim-majority powers. Fund establishment institutions and cite their declarations as diplomatic credentials. Adopting the rival universal-offensive reading would cost them treaties, aid, and security cooperation, so their practical options are bounded even where domestic constituencies press otherwise.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states, beneficiary,
    institutional, generational, constrained, global).

% Live under the security arrangements the reading underwrites: churches, temples, and communal institutions operate under guarantees framed as inviolable covenants. They lobby for consistent application and document violations where enforcement lapses. Emigration to non-Muslim-majority states is available to some but costly, splitting families and livelihoods.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, non_muslim_minority_communities, beneficiary,
    moderate, generational, constrained, regional).

% Encounter the reading's coercive edge only after breaching a treaty or initiating armed aggression: defensive coalitions form, sanctuary is withdrawn, and hostilities become lawful against them specifically. Ceasing aggression and honoring terms restores their protected standing. They bear no cost under the arrangement while compliant.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, treaty_breaking_aggressors, payer,
    moderate, immediate, constrained, regional).

% Operate outside the establishment's certified interpretive process, recruiting through transnational media and preaching that the contextual reading disarms the faithful against hostile powers. Members' ideological identity is fused with the rival abrogationist reading; participating in the establishment's deliberations would require surrendering the commitment that defines the movement.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, militant_rejectionist_movements, excluded,
    organized, generational, identity_locked, regional).

% Hold ordinary religious lives in plural societies: prayer, charity, neighborly cooperation with non-Muslims. The reading lets them treat scriptural fidelity and coexistence as compatible. The alternatives available to them, embracing militancy or leaving the faith, both carry severe personal costs, so they remain inside the arrangement by conviction reinforced by the absence of tolerable exits.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, devout_pluralist_believers, beneficiary,
    powerless, biographical, identity_locked, global).

% Documents the verse's occasion, the early treaty record, and the history of the abrogation debate from outside any confessional beneficiary set. Publishes philological and historiographic analyses that defenders and opponents of the reading alike cite; holds no enforcement role and bears none of its costs.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, academic_quranic_studies, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__contextual_defensive, mainstream_juridical_establishment).
narrative_ontology:fixing_cost_class(quran_9_5_scope__contextual_defensive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared rule for when force between Muslim and non-Muslim polities or communities is lawful: treaties bind whoever signs them regardless of religion, and hostilities require a prior breach or attack. This gives both sides a stable expectation that cooperation will not be exploited, addressing the mutual-fear problem that otherwise drives preemptive aggression.
% TRANSFER_FUNCTION: Moves war-authority from private or offensive initiative to centralized defensive response; moves security assurance to treaty partners and minority communities at the price of forgoing opportunistic conquest; and moves interpretive prestige, endowment income, and state patronage to the juridical institutions that certify the reading.
% ABSENT_VOICES: Militant rejectionist movements hold the rival abrogationist reading and sit outside the establishment's certified deliberative process; classical exegetes who recorded broad-abrogation positions are represented only through selection by modern compilers. The treaty-violator seat speaks only as a defendant after breach. All three would object that the certified process never heard them on equal terms.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight and 9:5's scope reverted to open contest, Muslim-majority states would lose the doctrinal credential underwriting treaties and alliances with non-Muslim powers; minority-community security guarantees would lose their juridical anchor; counter-radicalization frameworks built on establishment fatwas would lose their partner; and rejectionist recruitment would inherit an uncontested interpretive battlefield.
% FOUNDING_PROBLEM: Reconcile the sword verse's apparent breadth with the Qur'an's extensive peace and treaty verses and with the Prophet's own covenant practice, so that a plural seventh-century and subsequently modern environment could be governed without perpetually mandated religious war.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: secular academic quranic studies documents the occasional character of the verse's revelation and the seriousness of the medieval abrogation debate; historians of early Islam independently attest the treaty-breaking sequence the reading cites; international-law scholars note the reading's convergence with defensive-war doctrine. The rival readings' own proponents engage the same treaty texts, which attests the founding problem remains disputed-and-live rather than settled by self-interest.
narrative_ontology:disappearance_verdict(quran_9_5_scope__contextual_defensive, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__contextual_defensive, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__contextual_defensive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_9_5_scope__contextual_defensive, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__contextual_defensive, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__contextual_defensive_tests).
:- end_tests(quran_9_5_scope__contextual_defensive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.18) because the standing arrangement takes little from those it governs: compliant parties pay nothing, and the coercive edge triggers only on breach, so the victim set is breach-triggered rather than a steady extraction base. This is why the claim is rope despite a non-empty victim array. Suppression (0.28) is a raw structural property, unscaled by power or scope in the engine's arithmetic: it reflects refutation campaigns, curriculum control, and platform enforcement against rejectionist preaching, not physical coercion of compliant parties. Theater (0.22) is low-to-moderate: declarations and conferences do real coordinating work, but a growing share is performative endorsement outrunning practice. Accessibility collapse (0.45) is mid-range: within the reading's own premises the abrogationist alternative collapses, but the premises themselves are contested, so alternatives persist globally. Resistance (0.58) is substantial because rival readings contest the constraint continuously. All three temporal series share one grid (points 0 through 30, mapped to roughly 1995 through 2025); trajectories show mild extraction accumulation via state instrumentalization, rising theater via the declaration economy, and a rising suppression requirement as rivalry intensifies. No cyclical pattern is present; the drift is monotone and gradual.
 *
 * PERSPECTIVAL GAP:
 *   From the establishment seat the arrangement is a trust faithfully transmitted; from the state seat it is diplomatic infrastructure; from the minority seat it is a security guarantee experienced as reliable only where enforcement is consistent; from the treaty-breaker seat it is sudden lawful hostility; from the rejectionist seat it is a betrayal of the verse. One text, incompatible experiences, computed per seat from power, exit, and role data rather than averaged. Inter-institutionally, the establishment and the states are both institutional seats with different directionalities: the establishment administers and collects authority, the states consume certification and pay patronage. Identity-lock operates differently at the two locked seats: the establishment's lock is institutional (its authority has become the transmission it administers), while the rejectionists' lock is ideological (exit would dissolve the movement's defining commitment). If the establishment's identity frame broke, its seat would compute nearer a neutral observer; if the rejectionist frame broke, the excluded voice would empty and measured resistance would fall.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary seats (states, minority communities, pluralist believers) sit near the beneficiary end: the arrangement subsidizes their security and standing. The establishment is dual-positioned: collecting authority-rents pulls its d downward while bearing the cost of defending the reading against repudiation pressure pushes it upward; the net is mildly beneficiary-side, which the structural derivation captures without an override. The treaty_breaking_aggressors seat sits near the full-target end, but its exposure is conditional on its own breach, so its effective extraction is event-driven rather than continuous. The excluded seat contributes no directionality data; its objection registers through the resistance metric instead. No directionality_overrides were needed: beneficiary/victim declarations plus exit options produce the correct d for every seated agent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, reconciling the sword verse with the peace verses and the Prophet's treaty practice so intercommunal life stays governable, is still live, and the arrangement still performs that function. founding_problem_status is live and disappearance_verdict is world_rearranges, so the mismatch consumer finds no dead-mandate flag and no zombie condition. The mandatrophy risk here is not atrophy but instrumentalization: if state endorsement is convenience rather than conviction, the reading could persist as diplomatic theater while practice drifts. The theater_ratio series is the leading indicator to watch, and the instrumentalization_vs_conviction omega names the test that would resolve it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the quran_9_5_scope kernel: what would the sibling readings change structurally if they displaced it?',
    'Not resolvable by evidence alone: the siblings are live commitments held by different parties. Resolution arrives only as particular communities'' frameworks settle, trackable through institutional endorsements, curricular adoption, and fatwa practice.',
    'If abrogating_universal displaced this reading, the victim set expands from treaty violators to all unsubmitted polytheists and the beneficiary seats flip into mobilizers of offensive war. If progressive_synthesis displaced it, the victim set empties and the enforcement edge dissolves entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one of three live readings of the 9:5 scope kernel.').

omega_variable(
    abrogation_doctrine_hinge,
    'Does naskh (abrogation) operate between verse 9:5 and the peaceful verses, or is the abrogation claim methodologically unsupported?',
    'Intra-traditional juridical-method adjudication: whether the strict conditions for abrogation (explicit textual indication, later chronology, no harmonizing reading) are met, as documented in the usul al-fiqh literature and the madhhab positions.',
    'Affirming abrogation collapses this constraint into the abrogating_universal sibling; denying it stabilizes this reading and forces the progressive_synthesis sibling to argue from ethics rather than abrogation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogation_doctrine_hinge, conceptual, 'The doctrinal hinge on which the three readings divide.').

omega_variable(
    historical_referent_evidence,
    'Does the historical record (occasion-of-revelation reports, the treaty-breaking sequence of seventh-century Medina, the aftermath of Hudaybiyya) support restricting 9:5''s referent to the treaty-breaking tribes?',
    'Philological and historiographic analysis of the occasion-of-revelation corpus and early campaign records, including secular critical scholarship produced outside the confessional beneficiary set.',
    'Strong referent evidence hardens this reading against the abrogating_universal sibling on evidential terrain; weak or contested evidence leaves the scope question to framework choice rather than data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_referent_evidence, empirical, 'Whether the verse''s historical referent is determinable narrowly.').

omega_variable(
    instrumentalization_vs_conviction,
    'Is establishment and state endorsement of this reading settled conviction or diplomatic convenience?',
    'Observe endorsement behavior when utility inverts: if great-power patronage, tourism, or diplomacy incentives withdraw, do the endorsements persist?',
    'If instrumental, the reading''s persistence decouples from its doctrinal grounding and the theater_ratio series becomes the leading indicator of decay; if convicted, the reading is robust to incentive shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumentalization_vs_conviction, empirical, 'Whether the reading''s institutional support is conviction or convenience.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__contextual_defensive, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quran_9_5_contextual_defensive_tr_t0, quran_9_5_scope__contextual_defensive, theater_ratio, 0, 0.1).
narrative_ontology:measurement(quran_9_5_contextual_defensive_tr_t6, quran_9_5_scope__contextual_defensive, theater_ratio, 6, 0.12).
narrative_ontology:measurement(quran_9_5_contextual_defensive_tr_t12, quran_9_5_scope__contextual_defensive, theater_ratio, 12, 0.15).
narrative_ontology:measurement(quran_9_5_contextual_defensive_tr_t18, quran_9_5_scope__contextual_defensive, theater_ratio, 18, 0.18).
narrative_ontology:measurement(quran_9_5_contextual_defensive_tr_t24, quran_9_5_scope__contextual_defensive, theater_ratio, 24, 0.2).
narrative_ontology:measurement(quran_9_5_contextual_defensive_tr_t30, quran_9_5_scope__contextual_defensive, theater_ratio, 30, 0.22).

% Extraction over time
narrative_ontology:measurement(quran_9_5_contextual_defensive_be_t0, quran_9_5_scope__contextual_defensive, base_extractiveness, 0, 0.11).
narrative_ontology:measurement(quran_9_5_contextual_defensive_be_t6, quran_9_5_scope__contextual_defensive, base_extractiveness, 6, 0.13).
narrative_ontology:measurement(quran_9_5_contextual_defensive_be_t12, quran_9_5_scope__contextual_defensive, base_extractiveness, 12, 0.14).
narrative_ontology:measurement(quran_9_5_contextual_defensive_be_t18, quran_9_5_scope__contextual_defensive, base_extractiveness, 18, 0.16).
narrative_ontology:measurement(quran_9_5_contextual_defensive_be_t24, quran_9_5_scope__contextual_defensive, base_extractiveness, 24, 0.17).
narrative_ontology:measurement(quran_9_5_contextual_defensive_be_t30, quran_9_5_scope__contextual_defensive, base_extractiveness, 30, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(quran_9_5_contextual_defensive_su_t0, quran_9_5_scope__contextual_defensive, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(quran_9_5_contextual_defensive_su_t6, quran_9_5_scope__contextual_defensive, suppression_requirement, 6, 0.18).
narrative_ontology:measurement(quran_9_5_contextual_defensive_su_t12, quran_9_5_scope__contextual_defensive, suppression_requirement, 12, 0.21).
narrative_ontology:measurement(quran_9_5_contextual_defensive_su_t18, quran_9_5_scope__contextual_defensive, suppression_requirement, 18, 0.24).
narrative_ontology:measurement(quran_9_5_contextual_defensive_su_t24, quran_9_5_scope__contextual_defensive, suppression_requirement, 24, 0.26).
narrative_ontology:measurement(quran_9_5_contextual_defensive_su_t30, quran_9_5_scope__contextual_defensive, suppression_requirement, 30, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__contextual_defensive, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__progressive_synthesis).

% DUAL FORMULATION NOTE:
% Family decomposition per the epsilon-invariance principle: the colloquial label 'the meaning of 9:5' covers three structurally distinct constraints with different epsilon values, victim sets, and failure modes. This member (contextual_defensive) authors low epsilon over a narrow breach-triggered victim set; abrogating_universal authors high epsilon over a universal victim set; progressive_synthesis authors near-zero epsilon with an effectively empty victim set. Historical argument runs abrogating_universal, then contextual_defensive as its correction, then progressive_synthesis extending the contextualist turn; each later reading cites the earlier as the position it answers, so edges run mutually and contamination propagates in both directions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
