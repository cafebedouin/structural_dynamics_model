% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__sovereigntist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: rome_statute_jurisdiction__sovereigntist_reading
 *   human_readable: Rome Statute Sovereigntist Reading: Conditional ICC Jurisdiction via Consent
 *   domain: international_law/institutional_authority
 *
 * SUMMARY:
 *   The Rome Statute establishes the International Criminal Court as a forum
 *   for prosecuting genocide, crimes against humanity, and war crimes. Under
 *   the sovereigntist reading instantiated here, the statute's core
 *   architecture is a CONDITIONAL framework grounded in state consent: the
 *   ICC has jurisdiction only over nationals of states party to the treaty
 *   (Article 12(2)(a)), or over situations referred by the UNSC (Article
 *   13(b)), or when a non-party state accepts jurisdiction ad hoc (Article
 *   12(3)). This reading treats the consent requirement not as an unfortunate
 *   limitation but as a principled commitment to Westphalian sovereignty and
 *   treaty pacta sunt servanda. Nationals of non-party states (Russia, China,
 *   India, Myanmar, USA in certain contexts) are structurally immune from ICC
 *   jurisdiction absent UNSC action. National courts retain primary authority
 *   under the complementarity regime—the ICC defers to state prosecution, not
 *   as weakness but as structural principle. The sovereigntist reading
 *   vindicates two foundational propositions: (1) Westphalian sovereignty
 *   doctrine—states are the fundamental units of international law; (2) pacta
 *   sunt servanda—treaties bind only signatories. This reading is one of
 *   three structural interpretations of the same treaty text; sibling
 *   readings (universalist and hybrid-complementarity) contest whether the
 *   consent framework is legitimate or merely a political compromise that
 *   should yield to universal human rights imperatives.
 *
 * KEY AGENTS:
 *   - consenting_state_signatories: beneficiary and partial agenda-setter; retain gate control over ICC jurisdiction via consent and ASPP participation
 *   - national_judiciaries: beneficiary; the complementarity principle validates their primacy; ICC is structured as court of last resort
 *   - non_party_nationals: payer (powerless); systematically excluded unless UNSC refers; no direct access to ICC framework
 *   - vulnerable_populations_in_non_party_states: payer (powerless); mass atrocity victims in non-party states lack ICC recourse
 *   - UNSC_permanent_members: parallel agenda-setter; hold referral authority outside the consent framework; asymmetric power
 *   - ICC_prosecutor: constrained institutional actor; operates within state-consent boundaries; proprio motu authority limited to party states
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__sovereigntist_reading, 0.62).
domain_priors:suppression_score(rome_statute_jurisdiction__sovereigntist_reading, 0.41).
domain_priors:theater_ratio(rome_statute_jurisdiction__sovereigntist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__sovereigntist_reading, rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__sovereigntist_reading, "Rome Statute Sovereigntist Reading: Conditional ICC Jurisdiction via Consent").
narrative_ontology:topic_domain(rome_statute_jurisdiction__sovereigntist_reading, "international_law/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__sovereigntist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__sovereigntist_reading, '8653c9e2-bacf-4f6c-a759-b8c779d1b903').
narrative_ontology:cs_kernel_codification('8653c9e2-bacf-4f6c-a759-b8c779d1b903', formalized).
narrative_ontology:cs_authority_grounding('8653c9e2-bacf-4f6c-a759-b8c779d1b903', lineage).
narrative_ontology:cs_interpretation_layer_present('8653c9e2-bacf-4f6c-a759-b8c779d1b903').
narrative_ontology:cs_reading_relation('8653c9e2-bacf-4f6c-a759-b8c779d1b903', rome_statute_jurisdiction__universalist_reading, forecloses).
narrative_ontology:cs_reading_relation('8653c9e2-bacf-4f6c-a759-b8c779d1b903', rome_statute_jurisdiction__hybrid_complementarity_reading, coexists_with).
narrative_ontology:cs_axiom('8653c9e2-bacf-4f6c-a759-b8c779d1b903', foundational, state_consent_constitutive_requirement).
narrative_ontology:cs_axiom_status(state_consent_constitutive_requirement, holdable).
narrative_ontology:cs_axiom_grounding('8653c9e2-bacf-4f6c-a759-b8c779d1b903', state_consent_constitutive_requirement, conventional).
narrative_ontology:cs_axiom('8653c9e2-bacf-4f6c-a759-b8c779d1b903', foundational, pacta_sunt_servanda_binding).
narrative_ontology:cs_axiom_status(pacta_sunt_servanda_binding, holdable).
narrative_ontology:cs_axiom_grounding('8653c9e2-bacf-4f6c-a759-b8c779d1b903', pacta_sunt_servanda_binding, deontological).
narrative_ontology:cs_reference_frame('8653c9e2-bacf-4f6c-a759-b8c779d1b903', westphalian_state_primacy_framework).
narrative_ontology:cs_drift_state('8653c9e2-bacf-4f6c-a759-b8c779d1b903', contemporary_atrocity_landscape, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8653c9e2-bacf-4f6c-a759-b8c779d1b903', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, consenting_state_signatories).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, national_judiciaries).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, non_party_nationals).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, vulnerable_populations_in_non_party_states).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, westphalian_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, treaty_pacta_sunt_servanda_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States party to the Rome Statute retain the right to trigger or opt out of ICC jurisdiction. They define the boundaries of their consent and can withdraw (per Article 127). They frame the ICC as their institution, bound by their collective authorization. They participate in the Assembly of States Parties in setting the agenda.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, consenting_state_signatories, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__sovereigntist_reading, consenting_state_signatories, agenda_setter).

% Hold the primary duty to investigate and prosecute international crimes under the complementarity regime. The ICC is structured as a court of last resort, deferring to national systems. National judges retain authority and institutional prestige within the sovereigntist reading; the constraint validates their primacy.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, national_judiciaries, beneficiary,
    institutional, generational, constrained, national).

% Nationals of states not party to the Rome Statute receive no direct protection under the treaty framework. The ICC has no jurisdiction over them unless their state explicitly accepts jurisdiction or the UNSC refers the situation. They cannot petition for ICC involvement directly. Their recourse depends on whether their own national courts function and whether the international community perceives their case as urgent enough for UNSC action.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, non_party_nationals, payer,
    powerless, biographical, trapped, global).

% Victims of mass atrocity in non-party states (e.g., Myanmar, Russia, China, India nationals) are systematically excluded from ICC jurisdiction absent UNSC referral. The sovereigntist reading treats this exclusion as the legitimate price of respecting state consent. They have no standing to invoke the ICC's mandate and must depend on their national systems or diplomatic pressure.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, vulnerable_populations_in_non_party_states, payer,
    powerless, biographical, trapped, global).

% Hold a parallel jurisdiction pathway via referral authority (Article 13(b)). They can unilaterally place situations before the ICC regardless of state consent. They can also veto investigations (implicitly, by refusing to refer or by geopolitical pressure). Their power is asymmetric and seats them outside the consent framework while controlling access to ICC jurisdiction.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, unsc_permanent_members, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Can participate in ICC proceedings only as observers and submitting amici; they cannot initiate investigations or compel the Prosecutor to act. Under the sovereigntist reading, the ICC is bound by state consent, not by universal human rights mandates. They push for expansive jurisdiction but are structurally excluded from the gate-setting authority.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, human_rights_organizations, excluded,
    organized, generational, constrained, global).

% Argue for universal jurisdiction and the primacy of human rights over sovereignty. Under the sovereigntist reading they are locked out by the consent framework itself. They contest the reading's legitimacy but cannot access the decision-making apparatus; their objections are narrated as idealism vs. legal reality.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, international_criminal_justice_advocates, excluded,
    organized, generational, constrained, global).

% Operates within the jurisdictional boundaries set by the Rome Statute's consent architecture. Can initiate proprio motu investigations but only within the constraints of Article 13(c) and only for party states. Must navigate the complementarity principle as a deference mechanism, not as a barrier to be overridden. Their prosecutorial discretion is structurally bounded by state consent.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, icc_prosecutor, agenda_setter,
    institutional, biographical, constrained, global).

% The fixed text of the Rome Statute, which under the sovereigntist reading contains explicit consent requirements (Articles 12, 13, 14, 15) and the complementarity principle (Article 17). The reading treats the treaty text as authoritative and its consent architecture as intentional, not incidental.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_treaty_text, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_treaty_text).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__sovereigntist_reading, consenting_state_signatories).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__sovereigntist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared framework for states to voluntarily coordinate criminal accountability: signatory states delegate certain prosecutions to a common institution (ICC) when their domestic systems are unwilling or unable, while preserving state control over the scope and trigger of that delegation. Creates a transparent, documented system for inter-state agreement on international crime jurisdiction.
% TRANSFER_FUNCTION: Transfers prosecutorial authority from individual states to the ICC only when the state consents (explicitly or via UNSC referral). Moves resources and political capital from national judiciaries to international prosecutors, but only for consenting parties. Non-party nationals' justice claims are transferred to their own national systems (or nowhere, if those systems fail).
% ABSENT_VOICES: Nationals of major non-party states (Russia, China, India, USA as non-party for CAR situations, Myanmar), victims of atrocity in those states, and universalist human rights advocates who reject the consent framework are structurally excluded. They cannot petition the ICC and are not party to the treaty negotiation. Their objection would be that international criminal justice should transcend state boundaries and protect all humans equally.
% DISAPPEARANCE_RATIONALE: If the Rome Statute's consent framework disappeared and were replaced by universal ICC jurisdiction, the institutional landscape would reorganize: states would have to either withdraw from the treaty entirely or accept ICC investigation of their nationals without consent, national judiciaries would lose primacy, and the balance of prosecutorial power would shift from state control to international institutional control. The current system of state-by-state opt-in would become a universal mandate.
% FOUNDING_PROBLEM: Post-Cold War atrocities (Rwanda, Yugoslavia) revealed that individual states often lack the will or capacity to prosecute their own nationals for mass crimes. The Rome Statute was designed to create a backstop institution that states could voluntarily join, allowing them to delegate accountability to a neutral body while preserving state sovereignty. The treaty's consent architecture reflects a diplomatic compromise between universal accountability aspirations and state sovereignty concerns.
% FOUNDING_PROBLEM_CORROBORATION: Signatories and their legal representatives (state parties' governments, treaty negotiators, the International Court of Justice in advisory opinions) attest the founding problem remains live and that the consent framework is the appropriate response. Non-party states, universalist advocates, and UN human rights bodies attest the founding problem is only partially solved—consent-based architecture leaves massive gaps in coverage—and argue the problem demands universal jurisdiction. Empirical corroboration outside the benefiting parties: the 123-state non-party bloc, human rights NGOs, and academic consensus on the 'complementarity critique' all contest whether consent-based architecture adequately addresses the founding accountability problem.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__sovereigntist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__sovereigntist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__sovereigntist_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__sovereigntist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is INCREASING from 0.35 (1998 founding) to 0.62 (2026), tracking the cumulative effect of non-party state exemptions becoming more salient as atrocities in Russia, Myanmar, China, and other non-parties multiply without ICC remedy. The sovereigntist reading vindicates this rise as theoretically legitimate (consent is sacrosanct) but empirically costly. Suppression is MODERATE and INCREASING (0.25→0.41) because the constraint's persistence depends on active institutional maintenance: the ASPP reaffirms state primacy regularly, the Prosecutor navigates complementarity as deference not override, and the consent framework must be defended against universalist challenges. Theater is INCREASING (0.12→0.28) because as global atrocity coverage grows, the ICC's limited reach (123 party states, ~8 billion people in non-party jurisdictions) becomes harder to justify as pure coordination; increasingly the institution performs legitimacy (credible accountability for some) rather than solving the founding problem (universal accountability). Accessibility collapse is MODERATE-HIGH (0.68): once the consent framework is understood, alternatives are constrained—non-party nationals have no direct ICC recourse; UNSC referral is geopolitically contingent; national courts often fail or face pressure. One shared time grid across all three metrics: every time point measures all three, anchored to the treaty's founding (1998) and projected to 2026. The rising extractiveness and theater together suggest the constraint is drifting from rope (coordination mechanism) toward tangled_rope or even snare territory as the unmet demand from non-party populations grows and the consent barrier becomes more visible as extraction rather than principle.
 *
 * PERSPECTIVAL GAP:
 *   From a consenting state's perspective (e.g., Germany, Australia), the Rome Statute is a rope—a genuine coordination mechanism that they voluntarily joined and can voluntarily leave, with clear benefits (shared accountability burden, neutral institution). From a non-party national's perspective (e.g., a Syrian, Russian, or Burmese victim), the same statute is a SNARE—it excludes them from its protections without their consent and binds them to national courts that may perpetrate the crimes they seek redress for. The sovereigntist reading, by declaring the consent framework legitimate, effectively privileges the consenting-state perspective. A universalist reading would privilege the non-party perspective and reframe the statute as an incomplete and unjust constraint. The engine will compute per-seat classifications that expose this gap: consenting signatories' seat likely computes as rope or scaffold; non-party nationals' seat likely computes as snare or piton (persisting by institutional inertia despite failing its mandate). The gap is not reconcilable within one reading—it is the reading's fault line.
 *
 * DIRECTIONALITY LOGIC:
 *   Consenting state signatories are beneficiaries (d ≈ 0.25–0.35): they retain control, set the ASPP agenda, and face no jurisdiction risk outside what they consent to. National judiciaries benefit from primacy (d ≈ 0.20–0.30). Non-party nationals and vulnerable populations in non-party states are targets (d ≈ 0.85–0.95): they are systematically excluded, have no exit options (trapped), and bear the cost of non-prosecution. The UNSC permanent members occupy an anomalous seat: they are arbitrage-capable (can refer or not, selectively) yet constrained by the broader system, giving them d ≈ 0.40–0.60 (neither pure beneficiary nor pure target, but gate-holders with geopolitical leverage). The ICC Prosecutor sits at moderate power with constrained exit (d ≈ 0.55–0.65): operates within state-consent boundaries and cannot override them. The directionality derivation follows from: beneficiary/victim declarations + power atoms + exit options. Consenting parties have mobile exit (can withdraw per Article 127), so d moderates downward; non-party nationals are trapped with no alternative pathway, so d amplifies upward.
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereigntist reading faces a mandatrophy risk: the founding problem was stated as 'impunity for mass atrocity.' The consent framework was designed to solve it by enabling voluntary coordination among states willing to prosecute. But the founding problem persists DESPITE the Rome Statute: atrocities in non-party states go unprosecuted, UNSC referrals are geopolitically blocked, and victims in Myanmar, Sudan, and Palestine lack recourse. The mandate (ensure accountability for mass crime) has outlived the mechanism's efficacy—the statute is performing legitimacy (the ICC exists, it prosecutes in some cases) while failing to reduce the founding problem. The sovereigntist reading can sustain itself legally (the treaty text supports consent requirements) but faces a practical mandatrophy: the coordination it was meant to supply is not materializing for the majority-world population. Theater rising from 0.12 to 0.28 is the symptom: as the accountability gap widens, the ICC's performance legitimacy (visible trials, principled reasoning) must increase to maintain political support despite empirical failure. The mandatrophy is not YET resolved because consenting states still benefit from the arrangement and the UNSC geopolitical interests still prefer selective referral; but the constraint is under systematic stress from universalist criticism and demonstrated coverage gaps.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_legitimacy_vs_coverage,
    'Is the Rome Statute''s consent architecture a principled commitment to Westphalian sovereignty, or a geopolitically convenient mechanism that enables major powers to opt out of accountability?',
    'Historical intent analysis of treaty negotiators'' statements (found in preparatory documents and minutes of the Rome Conference 1998); survey of state motivations for joining vs. not joining; comparison of ICC case distribution against global atrocity geography. If non-party states cluster among geopolitical powers with histories of mass atrocity (Russia, China, USA, India, Myanmar), and if the stated reason for non-party status is sovereignty concerns rather than implementation capacity, the mechanism appears convenience-based rather than principled.',
    'If convenience-based, the sovereigntist reading''s claim to legitimacy weakens and the constraint drifts toward snare (extraction by powerful states of immunity). If principled, the consent architecture is defensible as coordinate mechanism. This omega directly bears on whether the constraint should be reclassified from rope to tangled_rope or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_legitimacy_vs_coverage, empirical, 'Whether the consent requirement is principled or geopolitically contingent.').

omega_variable(
    complementarity_deference_vs_obstruction,
    'Does the complementarity principle (Article 17) function as structural deference to national capacity, or as a barrier that ICC prosecutors use to avoid intervening in politically sensitive cases?',
    'Case-by-case analysis of ICC Prosecutor decisions to decline investigation; interview evidence from prosecutors on decision-making; comparison of ''complementarity-based declinations'' against independent assessments of national court capacity and political pressure. If most declinations occur in situations with weak national courts AND geopolitical sensitivity (cases involving powerful state allies), complementarity functions as obstruction; if declinations occur when genuine national capacity is present, it functions as deference.',
    'If obstruction, the complementarity principle is a mask for selective enforcement, and the constraint''s suppression and theater metrics are substantially higher than authored. If deference, the principle functions as intended. This bears on reclassification of the constraint from rope toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_deference_vs_obstruction, empirical, 'Whether complementarity is structural deference or strategic obstruction.').

omega_variable(
    reading_foreclosure_possibility,
    'Can the sovereigntist reading and the universalist reading coexist within the same legal framework, or does accepting one core premise logically foreclose the other?',
    'Formal logical analysis of the axioms declared in each reading. Sovereigntist axiom: ''state consent is a constitutive requirement of ICC jurisdiction.'' Universalist axiom (anticipated): ''universal human rights transcend state boundaries and entail ICC jurisdiction independent of consent.'' If these axioms are contradictory (both cannot be true in the same framework), the readings foreclose each other. If they can be harmonized (e.g., via a ''both/and'' framework that treats consent as procedural but universal rights as substantive), they coexist.',
    'If foreclosing, the engine''s cs_structure.reading_relations entry for this reading will be ''forecloses'' toward universalist. If coexisting, the relation is ''coexists_with''. This is a conceptual rather than empirical uncertainty and is resolved by analytic scrutiny of the axioms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_possibility, conceptual, 'Whether sovereigntist and universalist readings logically foreclose each other or coexist.').

omega_variable(
    extraction_vs_coordination_boundary,
    'Is the non-party-national exclusion a necessary price of coordination (coordination requires some boundary; those outside pay for those inside to have a functioning system), or is it pure extraction (the system could extend coverage without loss of function)?',
    'Functional analysis: conduct a counterfactual investigation of what would change if the ICC had universal jurisdiction over all humans regardless of state party status. If the institution could absorb universal caseload with proportional resource increases and the same complementarity/sovereignty logic, the exclusion is extractive. If extending coverage would necessarily degrade the institution''s function or violate core sovereignty commitments that signatories explicitly agreed to, the exclusion is a coordination cost.',
    'If extractive, the measured extractiveness should be higher and the constraint should drift toward snare. If coordination cost, the current metrics and rope classification are justified. This bears on whether the constraint is tangled_rope (coordination + extraction) or rope (pure coordination with bounded scope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, conceptual, 'Whether non-party exclusion is coordination cost or extractive boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__sovereigntist_reading, 1998, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement_basis(rome_tr_t1998, projected).
narrative_ontology:measurement(rome_tr_t2005, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2005, 0.16).
narrative_ontology:measurement_basis(rome_tr_t2005, observed).
narrative_ontology:measurement(rome_tr_t2010, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement_basis(rome_tr_t2010, observed).
narrative_ontology:measurement(rome_tr_t2015, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2015, 0.23).
narrative_ontology:measurement_basis(rome_tr_t2015, observed).
narrative_ontology:measurement(rome_tr_t2020, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2020, 0.26).
narrative_ontology:measurement_basis(rome_tr_t2020, observed).
narrative_ontology:measurement(rome_tr_t2026, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(rome_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 1998, 0.35).
narrative_ontology:measurement_basis(rome_be_t1998, projected).
narrative_ontology:measurement(rome_be_t2005, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement_basis(rome_be_t2005, observed).
narrative_ontology:measurement(rome_be_t2010, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement_basis(rome_be_t2010, observed).
narrative_ontology:measurement(rome_be_t2015, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2015, 0.56).
narrative_ontology:measurement_basis(rome_be_t2015, observed).
narrative_ontology:measurement(rome_be_t2020, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement_basis(rome_be_t2020, observed).
narrative_ontology:measurement(rome_be_t2026, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement_basis(rome_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 1998, 0.25).
narrative_ontology:measurement_basis(rome_su_t1998, projected).
narrative_ontology:measurement(rome_su_t2005, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2005, 0.31).
narrative_ontology:measurement_basis(rome_su_t2005, observed).
narrative_ontology:measurement(rome_su_t2010, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement_basis(rome_su_t2010, observed).
narrative_ontology:measurement(rome_su_t2015, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement_basis(rome_su_t2015, observed).
narrative_ontology:measurement(rome_su_t2020, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement_basis(rome_su_t2020, observed).
narrative_ontology:measurement(rome_su_t2026, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2026, 0.41).
narrative_ontology:measurement_basis(rome_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__sovereigntist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rome_statute_jurisdiction__sovereigntist_reading, 0.12).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, icc_complementarity_enforcement).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, unsc_referral_authority).

% DUAL FORMULATION NOTE:
% This constraint (sovereigntist_reading) is one of three readings of the contested kernel rome_statute_jurisdiction. The other readings—universalist_reading and hybrid_complementarity_reading—instantiate the same treaty text but generate different beneficiary/victim structures and extraction profiles. Sibling readings are linked via the kernel identifier and reading_relations in cs_structure. The sovereigntist reading emphasizes state consent as normatively legitimate; the universalist reading rejects consent as a barrier to universal human rights; the hybrid reading treats complementarity as a dynamic balance. Each reading is a separate constraint story with its own ε, its own stakeholders, and its own classification. The network edges here point to the siblings (must be authored in those files too) and to downstream constraints that the sovereigntist frame influences (ICC complementarity enforcement as a subordinate mechanism, UNSC referral authority as a parallel gate).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rome_statute_jurisdiction__sovereigntist_reading, powerful, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
