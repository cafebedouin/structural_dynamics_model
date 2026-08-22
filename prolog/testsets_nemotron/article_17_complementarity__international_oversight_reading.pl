% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__international_oversight_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__international_oversight_reading, []).

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
 *   constraint_id: article_17_complementarity__international_oversight_reading
 *   human_readable: Article 17 Complementarity — International Oversight Reading
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   The international oversight reading of Article 17 complementarity treats
 *   the ICC as an active guardian against impunity rather than a court of
 *   last resort. Under this reading, 'unwilling or unable' is interpreted
 *   broadly: domestic proceedings that lack genuine independence, proceed
 *   with manifestly inadequate intent, or serve as 'victor's justice' or
 *   elite shielding trigger ICC admissibility. The ICC's complementarity
 *   assessment becomes an accountability trigger — the mechanism that closes
 *   the impunity gap when states fail to pursue justice in good faith. This
 *   reading has gained procedural ground through OTP policy papers (2003,
 *   2010, 2019), Pre-Trial Chamber decisions (Lubanga, Gbagbo, Al Hassan),
 *   and the 2010 Kampala Review Conference amendments. The constraint
 *   extracts sovereignty prerogatives from states that maintain formal
 *   judicial systems but use them to shield powerful actors, while
 *   coordinating accountability for victims who would otherwise face
 *   impunity. The measured extractiveness reflects the sovereignty cost
 *   imposed on non-cooperating states; the coordination function is the
 *   impunity-gap closure for victims in complicit or failed states.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, 0.62).
domain_priors:suppression_score(article_17_complementarity__international_oversight_reading, 0.58).
domain_priors:theater_ratio(article_17_complementarity__international_oversight_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__international_oversight_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__international_oversight_reading, "Article 17 Complementarity — International Oversight Reading").
narrative_ontology:topic_domain(article_17_complementarity__international_oversight_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__international_oversight_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__international_oversight_reading, 'ed04d4b4-e9de-427e-a366-9ef8d854ce4a').
narrative_ontology:cs_kernel_codification('ed04d4b4-e9de-427e-a366-9ef8d854ce4a', formalized).
narrative_ontology:cs_authority_grounding('ed04d4b4-e9de-427e-a366-9ef8d854ce4a', lineage).
narrative_ontology:cs_interpretation_layer_present('ed04d4b4-e9de-427e-a366-9ef8d854ce4a').
narrative_ontology:cs_reading_relation('ed04d4b4-e9de-427e-a366-9ef8d854ce4a', article_17_complementarity__national_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('ed04d4b4-e9de-427e-a366-9ef8d854ce4a', foundational, complementarity_as_accountability_trigger).
narrative_ontology:cs_axiom_status(complementarity_as_accountability_trigger, holdable).
narrative_ontology:cs_axiom_grounding('ed04d4b4-e9de-427e-a366-9ef8d854ce4a', complementarity_as_accountability_trigger, conventional).
narrative_ontology:cs_axiom('ed04d4b4-e9de-427e-a366-9ef8d854ce4a', foundational, genuine_proceedings_require_independence_and_intent).
narrative_ontology:cs_axiom_status(genuine_proceedings_require_independence_and_intent, holdable).
narrative_ontology:cs_axiom_grounding('ed04d4b4-e9de-427e-a366-9ef8d854ce4a', genuine_proceedings_require_independence_and_intent, conventional).
narrative_ontology:cs_axiom('ed04d4b4-e9de-427e-a366-9ef8d854ce4a', secondary, victims_right_to_accountability_transcends_sovereignty).
narrative_ontology:cs_axiom_status(victims_right_to_accountability_transcends_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('ed04d4b4-e9de-427e-a366-9ef8d854ce4a', victims_right_to_accountability_transcends_sovereignty, deontological).
narrative_ontology:cs_reference_frame('ed04d4b4-e9de-427e-a366-9ef8d854ce4a', rome_statute_article_17_textual_complementarity).
narrative_ontology:cs_drift_state('ed04d4b4-e9de-427e-a366-9ef8d854ce4a', contemporary_icc_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ed04d4b4-e9de-427e-a366-9ef8d854ce4a', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__international_oversight_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, victims_in_complicit_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, victims_of_sham_proceedings).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, international_justice_institutions).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, sovereign_states_facing_icc_intervention).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, political_elites_shielded_by_domestic_courts).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, states_with_limited_capacity_but_genuine_will).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, icc_judicial_division).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, icc_office_of_the_prosecutor).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, states_with_limited_capacity_but_genuine_will).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, international_justice_civil_society).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, permanent_members_of_un_security_council).
narrative_ontology:constraint_vindicates(article_17_complementarity__international_oversight_reading, international_criminal_justice_as_complement_not_substitute).
narrative_ontology:constraint_vindicates(article_17_complementarity__international_oversight_reading, impunity_gap_closure_through_broad_unwilling_unable).
narrative_ontology:constraint_vindicates(article_17_complementarity__international_oversight_reading, victims_rights_to_accountability_transcend_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pre-Trial and Appeals Chambers interpret Article 17 through admissibility decisions (Lubanga, Gbagbo, Al Hassan, Said). They set the 'genuine proceedings' standard, define 'unwilling or unable,' and determine when domestic proceedings are sham or lack independence. Their interpretations expand or contract ICC jurisdiction. They benefit from institutional relevance and normative authority but are constrained by state cooperation and Security Council dynamics.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, icc_judicial_division, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__international_oversight_reading, icc_judicial_division, beneficiary).

% OTP initiates preliminary examinations, requests admissibility rulings, and drives complementarity policy (2003, 2010, 2019 policy papers). They benefit from expanded jurisdictional reach and operational relevance. Their selectivity in which situations to pursue shapes which complementarity-triggered situations actually produce accountability.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, icc_office_of_the_prosecutor, agenda_setter,
    institutional, biographical, analytical, global).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__international_oversight_reading, icc_office_of_the_prosecutor, beneficiary).

% Victims of international crimes in states where domestic courts are controlled by perpetrators or their allies. Domestic proceedings are unavailable, sham, or serve victor's justice. The ICC is their only structural pathway to accountability. They are trapped — cannot access domestic justice, depend entirely on ICC intervention, and have no exit from the territorial state's jurisdiction.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, victims_in_complicit_states, beneficiary,
    powerless, biographical, trapped, local).

% Victims where domestic proceedings exist but are performative — show trials, plea bargains that shield principals, proceedings that prosecute only low-level perpetrators while protecting architects. Their identity is often fused with the demand for genuine accountability; exit from this identity frame (accepting sham justice) is experienced as betrayal. The ICC's broad 'unwilling' test is their structural lifeline.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, victims_of_sham_proceedings, beneficiary,
    powerless, biographical, identity_locked, local).

% States (often non-party, sometimes party) whose domestic proceedings are deemed insufficient by the ICC. They bear sovereignty costs: loss of exclusive adjudicative authority, cooperation obligations (arrest, surrender, evidence), political exposure, and resource diversion. Their exit is constrained — treaty parties cannot withdraw from ongoing situations; non-parties face Security Council referral or territorial jurisdiction. Powerful states (US, Russia, China) use political and economic leverage to resist.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, sovereign_states_facing_icc_intervention, payer,
    powerful, biographical, constrained, national).

% Government officials, military commanders, and allied non-state actors who rely on domestic judicial systems to avoid accountability. The broad 'unwilling' interpretation directly threatens their immunity arrangements. They invest in maintaining domestic proceedings that satisfy formal complementarity while hollowing out substance — performative prosecutions, selective charging, judicial pressure. Their exit from the constraint's reach requires either genuine domestic accountability (which they avoid) or ICC non-intervention (which they lobby for).
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, political_elites_shielded_by_domestic_courts, payer,
    powerful, biographical, constrained, national).

% States that genuinely want to prosecute international crimes but lack resources, expertise, or institutional maturity. They face intensive ICC cooperation demands (Article 87), capacity-building conditionality, and the risk of being deemed 'unable' despite good faith. They benefit from ICC technical assistance and the complementarity framework's encouragement of domestic capacity, but bear disproportionate compliance costs relative to their capacity. Exit is constrained by treaty obligation and the impunity gap they seek to close.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, states_with_limited_capacity_but_genuine_will, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__international_oversight_reading, states_with_limited_capacity_but_genuine_will, beneficiary).

% NGOs, victim advocates, and legal networks that champion broad complementarity interpretation. They benefit from the framework's normative expansion, funding streams, and institutional access. They are mobile — can shift advocacy to other mechanisms (universal jurisdiction, regional courts) — but their organizational identity is fused with the ICC project.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, international_justice_civil_society, beneficiary,
    organized, generational, mobile, global).

% AU Peace and Security Council and Assembly have challenged complementarity as neo-colonial, demanded Article 98 non-surrender agreements, and called for withdrawal strategies. They would object to the broad interpretation but are structurally excluded from the ICC's judicial decision-making. Their exit is constrained — AU member states are mostly Rome Statute parties, and the AU lacks an alternative accountability mechanism.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, african_union_political_organs, excluded,
    organized, biographical, constrained, continental).

% P5 states (US, Russia, China, UK, France) — three non-parties, two parties. They control Security Council referrals (Article 13(b)) and deferrals (Article 16), shaping which complementarity-triggered situations reach the ICC. They bear reputational and geopolitical costs from ICC actions involving allies or themselves. They have arbitrage-grade exit: can veto referrals, negotiate Article 98 agreements, use bilateral pressure. Their observer seat is analytical but their structural power makes them de facto agenda-setters for the complementarity landscape.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, permanent_members_of_un_security_council, observer,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__international_oversight_reading, permanent_members_of_un_security_council, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Closes the impunity gap when states are unwilling or unable to genuinely prosecute international crimes. Provides a structural accountability trigger that activates international jurisdiction when domestic systems fail victims — whether through lack of capacity, lack of will, or active shielding of perpetrators through sham proceedings.
% TRANSFER_FUNCTION: Transfers adjudicative authority over international crimes from domestic courts (that fail the genuine proceedings test) to the ICC. Transfers sovereignty prerogatives (exclusive jurisdiction, prosecutorial discretion, judicial independence assessment) from territorial states to an international court. Transfers accountability outcomes from impunity/sham justice to potential prosecution for victims in complicit states.
% ABSENT_VOICES: Victims in situations where the ICC has declined to intervene despite complementarity triggers (e.g., Colombia preliminary examination closure, Afghanistan initial deferral, Venezuela Phase 1 closure). States that have withdrawn or threatened withdrawal (Burundi, Philippines, South Africa's aborted withdrawal) — their populations' voices on complementarity are mediated by political elites. Future generations who will inherit the precedent of broad vs. narrow complementarity.
% DISAPPEARANCE_RATIONALE: If the broad complementarity interpretation vanished overnight, the ICC would revert to a narrow 'court of last resort' — admissibility would require total domestic collapse, not lack of genuine intent. States with functional but captured courts would regain impunity for elites. Victims in complicit states would lose their only structural accountability pathway. The Rome Statute's impunity-gap closure function would be substantially hollowed out. The international criminal justice architecture would rearrange toward sovereignty primacy.
% FOUNDING_PROBLEM: The post-WWII/Nuremberg gap: sovereign states commit or tolerate international crimes with impunity because no permanent international court exists and domestic courts are either captured by perpetrators or lack capacity. The Rome Statute created the ICC as a complementarity-based court to close this gap — activating only when states fail to genuinely prosecute.
% FOUNDING_PROBLEM_CORROBORATION: The ICC and its civil society supporters attest the impunity gap persists and complementarity remains the essential trigger (OTP policy papers, Assembly of States Parties resolutions). Powerful non-party states and the African Union attest the founding problem is either solved (domestic systems now adequate) or was mischaracterized (complementarity was sold as sovereignty protection, not sovereignty displacement). Independent scholars (e.g., Schabas, Cryer, Nouwen) document the contested trajectory: the gap persists but the mechanism has drifted toward qualitative judicial review rather than gap-closure.
narrative_ontology:disappearance_verdict(article_17_complementarity__international_oversight_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__international_oversight_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__international_oversight_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(article_17_complementarity__international_oversight_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__international_oversight_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__international_oversight_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__international_oversight_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the substantial sovereignty transfer: states lose exclusive adjudicative authority over international crimes when domestic proceedings fail the 'genuine intent' test. Suppression (0.58) captures the coercive backbone — Security Council referrals, state cooperation obligations, and the threat of Article 87 non-compliance findings. Theater ratio (0.42) is significant: complementarity rhetoric often masks a de facto hierarchy where the ICC's admissibility test becomes a review of domestic judicial quality, not just existence. Accessibility collapse (0.48) is moderate — states can theoretically satisfy complementarity by conducting genuine proceedings, but the 'genuine' standard has expanded to encompass prosecutorial policy, judicial independence, and victim participation, making compliance structurally difficult for states with entrenched elite immunity. Resistance (0.71) is high: powerful states (US, Russia, China, India) reject the Court's jurisdiction; African Union has challenged complementarity as neo-colonial; non-cooperation is routine.
 *
 * PERSPECTIVAL GAP:
 *   From the ICC/institutional seat, the constraint is coordination: a necessary mechanism to prevent impunity gaps. From the sovereign state seat (especially non-party powerful states), it is extraction: an external body claiming authority over domestic judicial determinations. From the victim seat in a complicit state, it is a lifeline — the only structural avenue to accountability. From the elite immunity seat, it is an existential threat. The engine will compute these divergences from the declared structural positions; the claim (tangled_rope) asserts both coordination AND extraction are genuinely present, not that one is illusion.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC (as institutional agenda-setter) and international justice institutions are structural beneficiaries — they gain jurisdictional reach, normative authority, and operational relevance through broad complementarity interpretation. Victims in complicit states and victims of sham proceedings are the primary normative beneficiaries — they gain an accountability pathway otherwise foreclosed. Sovereign states facing ICC intervention are primary targets (payers) — they bear sovereignty costs, cooperation burdens, and political exposure. Political elites shielded by domestic courts are also targets — the constraint specifically threatens their immunity arrangements. States with limited capacity but genuine will occupy a complex seat: they are not extractive targets but face resource-intensive cooperation demands; their exit is constrained by treaty obligation and capacity gaps.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — closing impunity gaps when states fail — remains live (contested status). However, the constraint has accumulated extraction layers: the 'genuine proceedings' test has expanded from 'existence of proceedings' to qualitative assessment of judicial independence, prosecutorial policy, and victim access. This mission creep creates a mandate drift where the coordination function (gap-closure) now carries significant extraction (sovereignty displacement). The mandate has NOT been resolved — impunity gaps persist — but the extraction-to-coordination ratio has shifted toward extraction for states that maintain formal compliance while hollowing out substance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complementarity_coordination_extraction_boundary,
    'Where is the structural line between genuine complementarity coordination (closing impunity gaps) and sovereignty extraction (displacing domestic adjudication)?',
    'Comparative analysis of admissibility decisions: track how many states have been found ''unwilling/unable'' versus how many have satisfied complementarity through domestic proceedings; assess whether the standard has become impossible to satisfy for states with any political interference in prosecutions.',
    'If the line has shifted such that complementarity satisfaction is structurally unattainable for most states, the constraint reclassifies from tangled_rope toward snare; if genuine satisfaction remains achievable and common, the coordination function holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_coordination_extraction_boundary, conceptual, 'Whether the broad interpretation has made complementarity satisfaction a moving target that extracts sovereignty rather than coordinating accountability.').

omega_variable(
    victim_beneficiary_realization,
    'Do victims in complicit states actually access ICC justice through this mechanism, or is the victim-beneficiary claim largely symbolic given the ICC''s selective case load and state non-cooperation?',
    'Empirical tracking of victim participation and reparations outcomes in situations where complementarity triggered ICC jurisdiction (Uganda, DRC, CAR, Mali, Georgia, Burundi, Bangladesh/Myanmar, Afghanistan, Palestine, Ukraine, Venezuela, Philippines). Compare victims reached vs. victim populations in complementarity-triggered situations.',
    'If victim access is minimal relative to the victim populations the reading claims to benefit, the beneficiary declaration is overstated and the coordination function is largely performative — theater ratio should be higher, claimed type may shift toward piton or snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_beneficiary_realization, empirical, 'Whether the declared victim beneficiaries actually receive the accountability the reading promises.').

omega_variable(
    selectivity_as_extraction_mechanism,
    'Is the ICC''s situational selectivity (which complementarity-triggered situations get investigated) itself an extraction mechanism — serving powerful state interests while maintaining the complementarity framework''s legitimacy?',
    'Structural analysis of which complementarity-triggered situations receive OTP investigation vs. which are deferred; correlate with geopolitical alignment of the territorial state and Security Council dynamics.',
    'If selectivity patterns track powerful state preferences, the constraint''s extraction is not just sovereignty-displacement but also geopolitical — the complementarity framework becomes a managed gate for accountability that serves extractive interests. This would support a snare classification from the analytical seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_as_extraction_mechanism, conceptual, 'Whether complementarity''s operational selectivity undermines its coordination claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__international_oversight_reading, 2002, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(article_17_complementarity_oversight_tr_t2002, article_17_complementarity__international_oversight_reading, theater_ratio, 2002, 0.25).
narrative_ontology:measurement(article_17_complementarity_oversight_tr_t2006, article_17_complementarity__international_oversight_reading, theater_ratio, 2006, 0.28).
narrative_ontology:measurement(article_17_complementarity_oversight_tr_t2010, article_17_complementarity__international_oversight_reading, theater_ratio, 2010, 0.33).
narrative_ontology:measurement(article_17_complementarity_oversight_tr_t2014, article_17_complementarity__international_oversight_reading, theater_ratio, 2014, 0.38).
narrative_ontology:measurement(article_17_complementarity_oversight_tr_t2018, article_17_complementarity__international_oversight_reading, theater_ratio, 2018, 0.41).
narrative_ontology:measurement(article_17_complementarity_oversight_tr_t2022, article_17_complementarity__international_oversight_reading, theater_ratio, 2022, 0.42).

% Extraction over time
narrative_ontology:measurement(article_17_complementarity_oversight_be_t2002, article_17_complementarity__international_oversight_reading, base_extractiveness, 2002, 0.35).
narrative_ontology:measurement(article_17_complementarity_oversight_be_t2006, article_17_complementarity__international_oversight_reading, base_extractiveness, 2006, 0.42).
narrative_ontology:measurement(article_17_complementarity_oversight_be_t2010, article_17_complementarity__international_oversight_reading, base_extractiveness, 2010, 0.51).
narrative_ontology:measurement(article_17_complementarity_oversight_be_t2014, article_17_complementarity__international_oversight_reading, base_extractiveness, 2014, 0.57).
narrative_ontology:measurement(article_17_complementarity_oversight_be_t2018, article_17_complementarity__international_oversight_reading, base_extractiveness, 2018, 0.61).
narrative_ontology:measurement(article_17_complementarity_oversight_be_t2022, article_17_complementarity__international_oversight_reading, base_extractiveness, 2022, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(article_17_complementarity_oversight_su_t2002, article_17_complementarity__international_oversight_reading, suppression_requirement, 2002, 0.45).
narrative_ontology:measurement(article_17_complementarity_oversight_su_t2006, article_17_complementarity__international_oversight_reading, suppression_requirement, 2006, 0.48).
narrative_ontology:measurement(article_17_complementarity_oversight_su_t2010, article_17_complementarity__international_oversight_reading, suppression_requirement, 2010, 0.52).
narrative_ontology:measurement(article_17_complementarity_oversight_su_t2014, article_17_complementarity__international_oversight_reading, suppression_requirement, 2014, 0.55).
narrative_ontology:measurement(article_17_complementarity_oversight_su_t2018, article_17_complementarity__international_oversight_reading, suppression_requirement, 2018, 0.57).
narrative_ontology:measurement(article_17_complementarity_oversight_su_t2022, article_17_complementarity__international_oversight_reading, suppression_requirement, 2022, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__international_oversight_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_17_complementarity__international_oversight_reading, 0.12).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, article_17_complementarity__national_primacy_reading).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, rome_statute_article_87_cooperation).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, icc_prosecutorial_discretion_policy).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, un_security_council_referral_mechanism).

% DUAL FORMULATION NOTE:
% Article 17 complementarity decomposes into two readings with divergent ε values: the international_oversight_reading (this story, ε=0.62, claimed tangled_rope) treats complementarity as an accountability trigger with broad 'unwilling or unable' interpretation; the national_primacy_reading (sibling, distinct constraint) treats complementarity as a sovereignty shield with narrow interpretation and high ICC burden. They share the kernel (Rome Statute Article 17) but instantiate different constraints with different beneficiary/victim structures, different enforcement profiles, and different drift trajectories. The oversight reading extracts sovereignty from non-compliant states to coordinate victim accountability; the primacy reading coordinates state consent to preserve the Court's legitimacy. The oversight reading influences the primacy reading by expanding the admissibility jurisprudence that the primacy reading must contend with.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_17_complementarity__international_oversight_reading, institutional, 0.15).
constraint_indexing:directionality_override(article_17_complementarity__international_oversight_reading, powerful, 0.85).
constraint_indexing:directionality_override(article_17_complementarity__international_oversight_reading, organized, 0.35).
constraint_indexing:directionality_override(article_17_complementarity__international_oversight_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
