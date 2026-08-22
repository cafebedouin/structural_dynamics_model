% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__public_health_flexibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_ph_flexibility_reading, []).

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
 *   constraint_id: trips_agreement_interpretive_kernel__public_health_flexibility_reading
 *   human_readable: TRIPS Agreement Public Health Flexibility Reading
 *   domain: international/intellectual_property/public_health
 *
 * SUMMARY:
 *   The TRIPS Agreement (Trade-Related Aspects of Intellectual Property
 *   Rights) is a binding multilateral trade treaty that sets minimum IP
 *   standards across 164+ countries. The constraint examined here is ONE
 *   READING of a contested kernel — the TRIPS text itself. This reading
 *   (public_health_flexibility_reading) interprets TRIPS Articles 31
 *   (compulsory licensing) and 6 (parallel imports) as embedding broad,
 *   accessible flexibilities intended to preserve government authority to
 *   override patent rights during public health emergencies. The sibling
 *   reading (strong_exclusivity_reading) interprets the same text as
 *   mandating high, uniform patent protection with narrow, hard-to-invoke
 *   exceptions. The disagreement is not about the text's wording — it is
 *   about what that text was designed to protect and how much weight to give
 *   its exception clauses. This story instantiates the public health reading
 *   as a constraint: the standing arrangement under contest is the narrow,
 *   restrictively interpreted version of these flexibilities; this reading
 *   claims they are and should be broad. Metrics are authored for the
 *   reading's actual operation on the ground: how extractive the constraint
 *   is when the reading is institutionalized; how much enforcement is
 *   required to keep the flexibilities from being invoked; how much
 *   theatrical maintenance props up the narrow reading despite the text's
 *   apparent breadth.
 *
 * KEY AGENTS:
 *   - Generic manufacturers: organized actors who gain negotiating leverage and legal cover under this reading
 *   - Health ministries: institutional agenda-setters empowered to invoke compulsory licensing and parallel import authority
 *   - Pharmaceutical patent holders: powerful actors whose rents are taxed by the reading's interpretation
 *   - WTO dispute panels: institutional arbiters holding interpretive authority; their decisions determine which reading becomes operative
 *   - Developed-country trade representatives: institutional actors pressing for narrow flexibility interpretations on behalf of their innovator firms
 *   - LDC/developing-country coalitions: organized advocates for broad flexibility reading, constrained by trade system architecture
 *   - Patients in low-income countries: powerless beneficiaries trapped between unaffordable patents and uncertain government action
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.38).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.42).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "TRIPS Agreement Public Health Flexibility Reading").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "international/intellectual_property/public_health").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'c730b247-ecbd-42fc-8a17-708e2eaaa742').
narrative_ontology:cs_kernel_codification('c730b247-ecbd-42fc-8a17-708e2eaaa742', fixed_text).
narrative_ontology:cs_authority_grounding('c730b247-ecbd-42fc-8a17-708e2eaaa742', extraction).
narrative_ontology:cs_interpretation_layer_present('c730b247-ecbd-42fc-8a17-708e2eaaa742').
narrative_ontology:cs_reading_relation('c730b247-ecbd-42fc-8a17-708e2eaaa742', trips_agreement_interpretive_kernel__strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_axiom('c730b247-ecbd-42fc-8a17-708e2eaaa742', foundational, compulsory_licensing_broadly_justified).
narrative_ontology:cs_axiom_status(compulsory_licensing_broadly_justified, holdable).
narrative_ontology:cs_axiom_grounding('c730b247-ecbd-42fc-8a17-708e2eaaa742', compulsory_licensing_broadly_justified, deontological).
narrative_ontology:cs_axiom('c730b247-ecbd-42fc-8a17-708e2eaaa742', foundational, health_override_iprs_in_emergencies).
narrative_ontology:cs_axiom_status(health_override_iprs_in_emergencies, holdable).
narrative_ontology:cs_axiom_grounding('c730b247-ecbd-42fc-8a17-708e2eaaa742', health_override_iprs_in_emergencies, instrumental).
narrative_ontology:cs_reference_frame('c730b247-ecbd-42fc-8a17-708e2eaaa742', trips_negotiated_compromise_framework).
narrative_ontology:cs_drift_state('c730b247-ecbd-42fc-8a17-708e2eaaa742', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c730b247-ecbd-42fc-8a17-708e2eaaa742', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_manufacturers).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patients_in_resource_constrained_countries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_patent_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, least_developed_country_coalitions).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, developed_country_trade_representatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under the public health reading, generic manufacturers gain explicit legal grounds to produce patented medicines under compulsory licensing provisions (TRIPS Article 31) when a country declares a public health emergency. They can import and export generics across borders (parallel import, Article 6) without patent holder consent. This expands their market access and negotiating position relative to innovator firms. They remain constrained by the need for government authorization and by political pressure from patent holders; they cannot unilaterally declare public health emergencies.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_manufacturers, beneficiary,
    organized, biographical, constrained, global).

% Health ministries gain the reading's interpretation that they hold broad authority to invoke compulsory licensing and parallel import mechanisms to ensure population access to essential medicines at affordable prices. They are empowered as arbiters of what constitutes a public health emergency. They remain constrained by WTO dispute mechanics: a patent holder can challenge their licensing decision through dispute settlement, requiring legal defense and risking trade retaliation if they lose. The reading frames their authority; the dispute mechanism creates friction against its exercise.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries, agenda_setter).

% Under this reading, patients in low-income countries benefit from lower generic prices enabled by compulsory licensing and parallel imports. They cannot directly invoke the mechanism but depend on their governments to do so. Their benefit is contingent on political will within their health ministry and on their government's capacity to withstand trade retaliation threats. They remain trapped because patent-protected brand drugs at market prices are unaffordable; generics are their only realistic access pathway.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patients_in_resource_constrained_countries, beneficiary,
    powerless, immediate, trapped, national).

% Under this reading, pharmaceutical patent holders bear the cost of compulsory licensing and parallel imports: they lose market exclusivity in countries that invoke the flexibilities, face generic competition at lower prices, and see their negotiating position eroded. They are constrained because TRIPS Article 31 and Article 6 are written into the binding trade agreement; they cannot exit the system, only contest individual licenses through dispute settlement. Their rents are taxed by the reading's interpretation of what counts as a legitimate public health justification.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_patent_holders, payer,
    powerful, generational, constrained, global).

% WTO dispute panels hold the power to adjudicate challenges to compulsory licensing and parallel import decisions. Under this reading, they interpret TRIPS provisions to preserve government flexibility; under the strong exclusivity reading, they would narrow the flexibilities. Panels are constrained by the text itself and by the multilateral consensus that created TRIPS; they cannot unilaterally override the agreement. Their decisions shape which reading becomes operative on the ground.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, wto_dispute_panels, agenda_setter,
    institutional, generational, analytical, global).

% Trade representatives from developed countries where pharmaceutical IP is a major export industry experience the public health reading as unfavorable to their domestic innovator firms. They have constrained exit: they cannot withdraw TRIPS unilaterally without triggering retaliation across multiple trade domains. They exercise leverage through dispute settlement challenges and through bilateral trade negotiations that press for narrower flexibility interpretations.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, developed_country_trade_representatives, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__public_health_flexibility_reading, developed_country_trade_representatives, agenda_setter).

% Country coalitions (African Group, LDC Block) advocate for the broad public health reading in TRIPS negotiations and dispute settlement. They mobilize around compulsory licensing and parallel import rights as necessary to meet population health needs. Their exit is constrained: leaving TRIPS entirely would forfeit other trade benefits; they must work within the system to interpret flexibilities favorably.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, least_developed_country_coalitions, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__public_health_flexibility_reading, least_developed_country_coalitions, agenda_setter).

% The WTO dispute settlement system is the institutional apparatus through which competing readings of TRIPS are adjudicated. It is not itself an agent but the forum where agents contest interpretive authority.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, dispute_settlement_mechanism, observer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(trips_agreement_interpretive_kernel__public_health_flexibility_reading, dispute_settlement_mechanism).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_patent_holders).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__public_health_flexibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: TRIPS creates a multilateral framework for intellectual property protection that coordinates across 164+ member states to reduce barriers to trade in IP-dependent goods and services. The public health reading preserves coordination around a core innovation incentive while embedding explicit exit ramps: compulsory licensing (Article 31) and parallel imports (Article 6) enable countries to override patent exclusivity when facing public health emergencies, particularly for essential medicines. The coordination problem solved: absent TRIPS, countries would face bilateral pressure and fragmented IP rules; TRIPS centralizes the rule set and makes flexibilities transparent and defended by multilateral consensus.
% TRANSFER_FUNCTION: The constraint moves market exclusivity rights from generic manufacturers and health systems to pharmaceutical patent holders — except when health ministries invoke the Article 31 and Article 6 flexibilities, at which point the transfer reverses partially: generic manufacturers gain the right to produce and distribute, health systems gain the right to procure at generic prices, and patent holders lose exclusivity revenue in that jurisdiction. The reading interprets the exception as substantial and invokable, not merely theoretical.
% ABSENT_VOICES: Patients and health systems from low-income countries have limited direct voice in WTO proceedings, where seat allocation favors states with resources for legal teams. Excluded are: (1) individuals experiencing medicine rationing, (2) generic manufacturers from countries not seated at WTO, (3) public health advocacy organizations not formally recognized in dispute settlement. The absence of these voices shapes which arguments are heard: innovator-firm arguments dominate dispute proceedings because innovators are represented by wealthy-country governments, while patient-access arguments depend on whether health ministries or NGOs present them.
% DISAPPEARANCE_RATIONALE: If the public health flexibility reading disappeared and the strong exclusivity reading took its place globally, pharmaceutical patent holders would have narrower compulsory licensing grounds and parallel import would be severely constrained. Generic manufacturers would lose legal cover for production in emergency contexts; health ministries would lose negotiating leverage; medicine prices would rise in low-income countries; global treatment coverage for HIV, TB, and other endemic diseases would contract. The world would rearrange: patent holders would capture more of the global pharma market; generic capacity in India, Brazil, and other producing countries would shrink; supply chains for essential medicines would concentrate. If the reading vanished entirely (no TRIPS flexibilities, no strong exclusivity reading, just TRIPS's minimal text interpreted neutrally), countries would revert to bilateral negotiation and would likely issue compulsory licenses more freely; the reading structures their authority and its legal defensibility.
% FOUNDING_PROBLEM: TRIPS was negotiated in the 1990s under pressure from developed-country innovators to raise IP protection globally. The founding problems it was built to solve were: (1) countries free-riding on innovation by not protecting patents, reducing innovator incentives; (2) trade in counterfeit goods harming both innovators and consumers; (3) software and biotech industries seeking expanded rights. The public health flexibility reading emerged as a negotiating compromise: developed countries got higher IP floors, but developing countries embedded explicit exit ramps to protect population health. The compromise was codified in TRIPS Articles 31 and 6 and in the 2001 Doha Declaration affirming that health concerns override IP rules.
% FOUNDING_PROBLEM_CORROBORATION: Developed-country trade negotiators and pharmaceutical industry representatives attest that free-riding and counterfeiting remain live problems and that high IP protection is necessary to sustain innovation. Generic manufacturers, health ministries, and WHO representatives attest that access to medicines has become the dominant problem: millions die from treatable diseases because patents price them out, and the flexibilities are underused due to political pressure and legal uncertainty. Academic economists and epidemiologists external to both camps provide evidence that price elasticity is high for essential medicines in low-income countries and that compulsory licensing would substantially increase treatment coverage. The founding problem is contested because the beneficiary parties (innovators) and the victim parties (patients in poor countries) offer incommensurable framings of what problem the constraint is for.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__public_health_flexibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__public_health_flexibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).
:- end_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.38 because the public health reading substantially erodes patent holder rents — compulsory licensing and parallel imports enable generic production and undercut premium pricing. It is not higher (not 0.65+) because the flexibilities remain constrained by: (1) the need for formal government authorization, (2) WTO dispute settlement risk if a patent holder challenges the license, (3) political pressure on health ministries from trade partners. Suppression is 0.42 because enforcement is required to keep the narrow reading alive: developed countries must apply diplomatic pressure and dispute settlement threats; pharmaceutical industry must fund legal challenges and maintain lobbying presence; multilateral institutions must maintain interpretive ambiguity. The suppression has declined over time (from 0.75 in 1995 to 0.42 in 2021-26) because the public health reading has gained legitimacy through: the 2001 Doha Declaration, the 2007 TRIPS amendment enabling LDC manufacturing, WHO advocacy, academic consensus supporting flexibility, and demonstrated generic capacity in India and Brazil. Theater ratio is low (0.28) and rising slowly: the functional core (actual licensing, parallel import usage) remains modest relative to what the reading's authority suggests should be possible. Theater is rising because countries increasingly invoke the reading rhetorically without implementing mechanisms (declarations of public health emergencies without actual licenses issued; dispute settlement threats without follow-through). The measurement series span the interval from TRIPS signature (1995) through Doha (2001), through the HIV/AIDS generic access movement (2008), through contemporary (2026). All metrics are authored on the same time grid.
 *
 * PERSPECTIVAL GAP:
 *   The pharmaceutical patent holder seat (and the developed-country trade representative seat aligned with it) would compute a different constraint type than the generic manufacturer and health ministry seats. From the patent-holder perspective, TRIPS imposes a coordination obligation to respect high IP floors; the flexibilities appear as narrow exceptions creating friction and legal risk. They would classify this as a snare on their interests: they are locked into a binding multilateral commitment that constrains their pricing power and enables regulatory workarounds. From the generic manufacturer and health ministry seats, TRIPS coordinates a minimum IP floor but preserves their authority to override it; they see the constraint as a tangled rope: coordination (the unified global IP framework) plus asymmetric payoff (they bear compliance costs but gain exception rights). The engine computes these divergent types from the structural data: patent holders as powerful actors with constrained exit (cannot leave TRIPS unilaterally), high extraction burden (lose rents when compulsory licenses issue), and victim status. Generic manufacturers and health ministries as beneficiaries with organized power, constrained but not trapped exit (can invoke flexibilities; risk trade retaliation but retain legal cover), and lower extraction burden (gain access rights, bear compliance costs of licensing procedures).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality: generic manufacturers (organized, constrained exit) and health ministries (institutional, constrained exit, agenda-setter role) derive d near 0.3 — they benefit from the reading without capturing it fully (they depend on dispute outcomes they do not control and on political will to invoke the flexibilities). Patients (powerless, trapped exit) derive d near 0.15 despite beneficiary status because they are structurally dependent on governments to exercise authority on their behalf. Pharmaceutical patent holders (powerful, constrained exit, victim status) derive d near 0.85 — they are the structural targets: they lose rents, face market exclusivity erosion, and cannot exit the regime. WTO dispute panels (institutional, analytical exit) derive d at 0.5 — they are neutral arbiters, neither collecting from nor bearing the constraint. Developed-country trade representatives derive d near 0.65 — they pay through lost negotiating leverage on behalf of their pharma constituents but are not direct victims themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is PRESENT but NOT RESOLVED on the ground. The founding problem (ensuring access to medicines while incentivizing innovation) was real in 1995 and remains partially live. The reading claims the flexibilities solve access while preserving innovation incentives. The measurement data show extractiveness declining from 0.68 to 0.38 over 31 years, which is consistent with successful mitigation of the extraction problem: as compulsory licensing and parallel import norms normalized, the reading's authority constrained patent holders' ability to extract monopoly rents. However, mandatrophy has NOT resolved because: (1) political invocation of the flexibilities remains low relative to the reading's authority (theater_ratio is 0.28, not near 0 — the machinery is partially unused or performatively invoked), (2) dispute settlement remains a live barrier: countries fear challenging patent holders; (3) the reading's authority is NOT universally accepted (the strong exclusivity reading remains live among developed-country trade negotiators and some dispute panels). The constraint persists because neither the beneficiary parties (generic manufacturers, health ministries) nor the victim parties (patent holders) have interest in resolving the mandate: beneficiaries benefit from the ambiguous, high-theater state (they have legal cover without committing to full implementation); victim parties benefit from the suppression required to keep the reading from reaching its full authority (they maintain negotiating leverage by pressing for strict interpretation). True mandatrophy resolution would require either: (a) the reading to become completely institutionalized and uncontested (flexibilities invoked as routine, no dispute settlement challenges, integration into standard practice), or (b) the strong exclusivity reading to definitively displace it (flexibilities narrowed, compulsory licensing severely restricted). Neither has occurred.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_text_interpretation_ambiguity,
    'Does the TRIPS text actually embed broad, accessible compulsory licensing and parallel import flexibilities intended for public health, or narrow exceptions to a high IP protection regime?',
    'Close reading of negotiation history, statements of parties during Uruguay Round, and explicit language of Articles 31 and 6. The Doha Declaration (2001) provided one authoritative interpretation, but textual ambiguity persists: words like ''emergency'' and ''public health'' are not defined in the text itself.',
    'If the text''s intent was genuinely broad, the public health reading is validated and the strong exclusivity reading is a misreading. If the intent was narrow (flexibilities as exceptions proving the IP protection rule), the strong exclusivity reading prevails and compulsory licensing becomes harder to justify. This directly determines which reading is classified as natural law vs. constructed extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_text_interpretation_ambiguity, conceptual, 'Whether the TRIPS text structurally embeds broad or narrow public health flexibilities').

omega_variable(
    dispute_settlement_interpretive_closure,
    'Will WTO dispute panels ultimately adopt the public health reading or the strong exclusivity reading as the binding interpretation?',
    'Observation of pending and future dispute settlement cases challenging compulsory licenses and parallel imports. The 2023 TRIPS-COVID waiver and its institutional aftermath will shape precedent. A series of panel decisions upholding broad compulsory licensing authority (or narrowing it severely) will resolve this.',
    'If panels adopt the public health reading durably, suppression will continue declining and the reading will fully institutionalize. If panels adopt the strong exclusivity reading, extractiveness will rise, suppression will remain high, and the public health reading will be foreclosed. This determines the long-term classification: rope (if public health dominates) vs. snare (if exclusivity dominates).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dispute_settlement_interpretive_closure, empirical, 'Which reading will WTO dispute settlement enshrine as binding interpretation').

omega_variable(
    generic_manufacturing_capacity_constraint,
    'Is the underuse of compulsory licensing driven by legal ambiguity and suppression (as the public health reading claims), or by lack of manufacturing capacity and government unwillingness to invoke the flexibilities (as critics of the reading argue)?',
    'Empirical study of countries with genuine manufacturing capacity (India, Brazil) and documented public health emergencies (HIV in South Africa, TB in low-income countries): did they issue compulsory licenses where capacity existed and barriers were removed? The India–US bilateral pressures and the 2007 TRIPS amendment offer natural experiments.',
    'If underuse is driven by suppression (legal barriers, trade pressure, political fear), the public health reading is correct that flexibilities are broad but institutionally constrained. If underuse is driven by incapacity or unwillingness, the reading''s claim that flexibilities are ''broad'' becomes empirically questionable. This determines whether the theater_ratio is high because the reading is performative (unsupported by real implementation) or because the reading''s authority is suppressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generic_manufacturing_capacity_constraint, empirical, 'Whether low invocation of compulsory licensing reflects legal suppression or practical constraints').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Are the public health and strong exclusivity readings logically foreclosed by each other (cannot both be true in any single coherent framework), or do they coexist as different parties'' live positions?',
    'Formal analysis of the textual contradiction: if ''broad flexibilities'' and ''narrow exceptions'' truly contradict, the readings foreclose. If they can be reconciled as different emphasis or different scope (e.g., broad in principle but narrow in practice due to trade pressure), they coexist. The Doha Declaration framed them as coexisting; some legal scholars argue they foreclose.',
    'If they foreclose, one reading will eventually displace the other through dispute settlement and institutional evolution. If they coexist, both will persist as live contestation. This determines whether the constraint will eventually stabilize (foreclosure path) or remain in dynamic tension (coexistence path). A foreclosure finding implies eventual classification shift; coexistence implies stable tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the two readings are logically foreclosed or can coexist').

omega_variable(
    suppression_mechanism_internationalization,
    'Is suppression of the public health reading maintained primarily through: (a) WTO dispute settlement threat (if a country invokes compulsory licensing, the patent holder can challenge and trigger trade retaliation), (b) bilateral trade pressure (developed countries threaten trade sanctions), (c) ideological/institutional capture (health ministries internalize the belief that patents are legally unquestionable), or (d) practical barriers (lack of manufacturing capacity, lack of government technical capacity to administer compulsory licenses)?',
    'Study of countries that have invoked compulsory licenses (Brazil, Thailand, India) and countries that have not despite having domestic public health emergencies and manufacturing capacity. Comparative analysis of institutional barriers, legal threats, trade pressure, and ideological factors. Post-2001 Doha trajectory shows variation: suppression intensity differs by country, suggesting mechanisms are mixed.',
    'If suppression is primarily institutional/legal (dispute settlement threat, trade pressure), it is reversible through institutional reform. If suppression is primarily internalized (health ministries believe patents are unquestionable), it is more durable and requires ideological shift. This informs whether the theater_ratio will continue rising (if suppression is durable) or whether implementation could accelerate (if suppression is institutional friction that can be overcome).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internationalization, empirical, 'What mechanisms maintain suppression of the public health reading''s implementation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 1995, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 1995, 0.05).
narrative_ontology:measurement(trip_tr_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2001, 0.12).
narrative_ontology:measurement(trip_tr_t2008, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement(trip_tr_t2015, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(trip_tr_t2021, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2021, 0.28).
narrative_ontology:measurement(trip_tr_t2026, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 1995, 0.68).
narrative_ontology:measurement(trip_be_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2001, 0.55).
narrative_ontology:measurement(trip_be_t2008, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2008, 0.48).
narrative_ontology:measurement(trip_be_t2015, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(trip_be_t2021, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2021, 0.38).
narrative_ontology:measurement(trip_be_t2026, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2026, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 1995, 0.75).
narrative_ontology:measurement(trip_su_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2001, 0.68).
narrative_ontology:measurement(trip_su_t2008, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2008, 0.58).
narrative_ontology:measurement(trip_su_t2015, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement(trip_su_t2021, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2021, 0.42).
narrative_ontology:measurement(trip_su_t2026, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2026, 0.42).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1995, tn=2026
narrative_ontology:measurement(trip_grid_01, trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse(class), 1995, 0.78).
narrative_ontology:measurement(trip_grid_02, trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse(class), 2026, 0.58).
narrative_ontology:measurement(trip_grid_03, trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse(individual), 1995, 0.85).
narrative_ontology:measurement(trip_grid_04, trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse(individual), 2026, 0.62).
narrative_ontology:measurement(trip_grid_05, trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse(organizational), 1995, 0.92).
narrative_ontology:measurement(trip_grid_06, trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse(organizational), 2026, 0.71).
narrative_ontology:measurement(trip_grid_07, trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse(structural), 1995, 0.88).
narrative_ontology:measurement(trip_grid_08, trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse(structural), 2026, 0.68).
narrative_ontology:measurement(trip_grid_09, trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance(class), 1995, 0.22).
narrative_ontology:measurement(trip_grid_10, trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance(class), 2026, 0.71).
narrative_ontology:measurement(trip_grid_11, trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance(individual), 1995, 0.12).
narrative_ontology:measurement(trip_grid_12, trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance(individual), 2026, 0.68).
narrative_ontology:measurement(trip_grid_13, trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance(organizational), 1995, 0.18).
narrative_ontology:measurement(trip_grid_14, trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance(organizational), 2026, 0.74).
narrative_ontology:measurement(trip_grid_15, trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance(structural), 1995, 0.15).
narrative_ontology:measurement(trip_grid_16, trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance(structural), 2026, 0.71).
narrative_ontology:measurement(trip_grid_17, trips_agreement_interpretive_kernel__public_health_flexibility_reading, stakes_inflation(class), 1995, 0.81).
narrative_ontology:measurement(trip_grid_18, trips_agreement_interpretive_kernel__public_health_flexibility_reading, stakes_inflation(class), 2026, 0.55).
narrative_ontology:measurement(trip_grid_19, trips_agreement_interpretive_kernel__public_health_flexibility_reading, stakes_inflation(individual), 1995, 0.72).
narrative_ontology:measurement(trip_grid_20, trips_agreement_interpretive_kernel__public_health_flexibility_reading, stakes_inflation(individual), 2026, 0.48).
narrative_ontology:measurement(trip_grid_21, trips_agreement_interpretive_kernel__public_health_flexibility_reading, stakes_inflation(organizational), 1995, 0.85).
narrative_ontology:measurement(trip_grid_22, trips_agreement_interpretive_kernel__public_health_flexibility_reading, stakes_inflation(organizational), 2026, 0.62).
narrative_ontology:measurement(trip_grid_23, trips_agreement_interpretive_kernel__public_health_flexibility_reading, stakes_inflation(structural), 1995, 0.79).
narrative_ontology:measurement(trip_grid_24, trips_agreement_interpretive_kernel__public_health_flexibility_reading, stakes_inflation(structural), 2026, 0.51).
narrative_ontology:measurement(trip_grid_25, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression(class), 1995, 0.76).
narrative_ontology:measurement(trip_grid_26, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression(class), 2026, 0.42).
narrative_ontology:measurement(trip_grid_27, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression(individual), 1995, 0.68).
narrative_ontology:measurement(trip_grid_28, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression(individual), 2026, 0.38).
narrative_ontology:measurement(trip_grid_29, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression(organizational), 1995, 0.81).
narrative_ontology:measurement(trip_grid_30, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression(organizational), 2026, 0.48).
narrative_ontology:measurement(trip_grid_31, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression(structural), 1995, 0.75).
narrative_ontology:measurement(trip_grid_32, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression(structural), 2026, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__public_health_flexibility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.12).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel__strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_amendment_2007_ldcs).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, wto_dispute_settlement_pharmaceuticals).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, doha_declaration_public_health).

% DUAL FORMULATION NOTE:
% The TRIPS Agreement kernel is instantiated by at least two structurally distinct constraints: this story (public_health_flexibility_reading) and the strong_exclusivity_reading. The two readings interpret the same TRIPS text differently, yielding different beneficiary/victim sets, different extractiveness profiles, and different types. They are not two measurements of one constraint; they are two constraints grounded in competing interpretations of the same legal artifact. The epsilon values differ substantially (0.38 for the public health reading vs. projected 0.72+ for the strong exclusivity reading) because the readings differ on what constitutes extractiveness: under the public health reading, compulsory licensing mitigates extraction; under the strong exclusivity reading, it is a deviation from the justified extraction (innovation incentive). Link both stories via network.affects_constraints to document their structural interdependence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__public_health_flexibility_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
