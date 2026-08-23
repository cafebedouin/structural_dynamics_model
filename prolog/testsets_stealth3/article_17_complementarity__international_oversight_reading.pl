% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__international_oversight_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: article_17_complementarity__international_oversight_reading
 *   human_readable: ICC Complementarity Trigger — International Oversight Reading
 *   domain: legal/international/criminal_justice
 *
 * SUMMARY:
 *   Under the international-oversight reading, Article 17 complementarity
 *   operates as an accountability-trigger mechanism rather than a sovereignty
 *   shield: the ICC is the guardian of last resort against impunity, and
 *   'unwilling or unable' is construed broadly so that victor's justice,
 *   elite immunity deals, symbolic prosecutions of low-level scapegoats, and
 *   complicit or collapsed judiciaries all count as failure triggering
 *   international jurisdiction. The standing arrangement this story is about
 *   is that broad-threshold regime itself. Epsilon is authored for that
 *   referent as THIS reading assesses it: the transfer of adjudicative
 *   authority is priced as a legitimate accountability cost borne chiefly by
 *   culpable or failed guardians, tempered by real institutional costs to
 *   state judicial systems and by capacity-limited, asymmetric reach — hence
 *   a moderate 0.40 rather than the near-zero a triumphalist account or the
 *   near-maximum the sibling reading's seat would author. Per Rule 1, the
 *   contest with the national-primacy reading lives in the omega variables
 *   and cs_structure, not in hedged metrics. KEY AGENTS (by structural
 *   relationship): - icc_chambers_and_prosecutor: Agenda setter
 *   (institutional/mobile) — administers the trigger, collects expanded
 *   docket and mandate - atrocity_victims_in_complicit_states: Primary
 *   beneficiary (powerless/trapped) — receives the escalation path -
 *   human_rights_advocacy_network: Secondary beneficiary (organized/mobile) —
 *   mobilizes the trigger - complicit_state_executives: Primary target
 *   (powerful/constrained) — bears immunity-stripping exposure -
 *   national_judicial_establishments_of_targeted_states: Dual
 *   payer/beneficiary (institutional/constrained) — loses final say, gains
 *   capacity support - great_power_non_parties: Excluded critic
 *   (institutional/arbitrage) — shapes the mechanism while rejecting its
 *   premise - transitional_justice_scholars: Analytical observer
 *   (moderate/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, 0.4).
domain_priors:suppression_score(article_17_complementarity__international_oversight_reading, 0.58).
domain_priors:theater_ratio(article_17_complementarity__international_oversight_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__international_oversight_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__international_oversight_reading, "ICC Complementarity Trigger — International Oversight Reading").
narrative_ontology:topic_domain(article_17_complementarity__international_oversight_reading, "legal/international/criminal_justice").

domain_priors:requires_active_enforcement(article_17_complementarity__international_oversight_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__international_oversight_reading, '044f0581-d093-48f6-bf03-56398692aa69').
narrative_ontology:cs_kernel_codification('044f0581-d093-48f6-bf03-56398692aa69', fixed_text).
narrative_ontology:cs_authority_grounding('044f0581-d093-48f6-bf03-56398692aa69', lineage).
narrative_ontology:cs_interpretation_layer_present('044f0581-d093-48f6-bf03-56398692aa69').
narrative_ontology:cs_reading_relation('044f0581-d093-48f6-bf03-56398692aa69', article_17_complementarity__national_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('044f0581-d093-48f6-bf03-56398692aa69', foundational, impunity_by_state_failure_triggers_international_jurisdiction).
narrative_ontology:cs_axiom_status(impunity_by_state_failure_triggers_international_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('044f0581-d093-48f6-bf03-56398692aa69', impunity_by_state_failure_triggers_international_jurisdiction, deontological).
narrative_ontology:cs_axiom('044f0581-d093-48f6-bf03-56398692aa69', foundational, sham_and_victors_justice_constitute_unwillingness).
narrative_ontology:cs_axiom_status(sham_and_victors_justice_constitute_unwillingness, holdable).
narrative_ontology:cs_axiom_grounding('044f0581-d093-48f6-bf03-56398692aa69', sham_and_victors_justice_constitute_unwillingness, empirically_contingent).
narrative_ontology:cs_reference_frame('044f0581-d093-48f6-bf03-56398692aa69', accountability_trigger_framework).
narrative_ontology:cs_drift_state('044f0581-d093-48f6-bf03-56398692aa69', contemporary_noncooperation_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('044f0581-d093-48f6-bf03-56398692aa69', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__international_oversight_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, atrocity_victims_in_complicit_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, human_rights_advocacy_network).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, state_parties_with_functional_judiciaries).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, complicit_state_executives).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, national_judicial_establishments_of_targeted_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, national_judicial_establishments_of_targeted_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides when to open preliminary examinations, makes admissibility determinations under Article 17, issues arrest warrants, and negotiates cooperation with states. Each broadening of the trigger adds situations, staff, and budget lines to the office; each refusal to act draws legitimacy criticism from the advocacy network. Its discretion to open or close situations is its main lever, but it cannot itself compel arrests or seize suspects.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, icc_chambers_and_prosecutor, agenda_setter,
    institutional, generational, mobile, global).

% Survivors and families of mass atrocity in states whose courts cannot or will not prosecute the perpetrators. The broad trigger gives their claims an escalation path that bypasses the complicit state entirely: a communication filed by others can become a formal investigation over their government's objection. Their access runs through NGOs and counsel; they cannot themselves summon the Court, and when states refuse cooperation their cases stall for years or decades.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, atrocity_victims_in_complicit_states, beneficiary,
    powerless, generational, trapped, global).

% NGOs, victim lawyers, forensic documentation projects, and coalition campaigns that file communications, publish evidence dossiers, and lobby the Assembly of States Parties. The broad reading multiplies the situations they can credibly push onto the docket and the funding their documentation attracts. They can shift attention across crises but hold no enforcement power of their own.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, human_rights_advocacy_network, beneficiary,
    organized, biographical, mobile, global).

% Heads of state, security chiefs, and allied elites whose immunity the broad reading is built to strip. Warrants restrict travel, freeze assets reachable in cooperating jurisdictions, and convert domestic impunity into permanent international exposure. They command their own state machinery and can obstruct investigations at home, but cannot remove an existing warrant except through cooperation or losing power; some evade by sheltering in non-party territories.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, complicit_state_executives, payer,
    powerful, biographical, constrained, national).

% Justice ministries, attorneys general, and courts of states under examination. When the Court finds their processes insufficiently genuine they lose final say over their own suspects; simultaneously, positive-complementarity policy channels training, legislative drafting help, and donor funds into their institutions. Their professional standing is fused with sovereignty claims, so deference to The Hague registers as institutional humiliation even where the material support is welcome.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, national_judicial_establishments_of_targeted_states, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__international_oversight_reading, national_judicial_establishments_of_targeted_states, beneficiary).

% Major powers outside the treaty, and signatories that never ratified, which denounce the broad trigger as illegitimate interference while shaping it through Security Council referrals, funding leverage, hosting negotiations, and occasional sanctions on Court personnel. They can shield themselves and their allies from the trigger's practical reach while endorsing accountability rhetoric aimed at adversaries.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, great_power_non_parties, excluded,
    institutional, generational, arbitrage, global).

% International law academics and practitioner-commentators who audit admissibility rulings, propose genuineness criteria, and supply the interpretive vocabulary that both the Court and its critics deploy. They bear none of the mechanism's costs and collect citation and advisory capital from every controversy the trigger generates.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, transitional_justice_scholars, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_17_complementarity__international_oversight_reading, icc_chambers_and_prosecutor).
narrative_ontology:fixing_cost_class(article_17_complementarity__international_oversight_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a backstop adjudicator for the impunity gap: when the state that would ordinarily prosecute atrocity crimes is unable (collapsed institutions) or unwilling (complicit), the trigger converts the gap into an international docket, and positive-complementarity pressure pushes capacity-building into national systems so the gap closes from below.
% TRANSFER_FUNCTION: Moves adjudicative authority over atrocity suspects from the government of the territory state to the Court; moves arrest, extradition, and cooperation obligations onto state parties and Security Council-referred states; moves procedural standing and recognition to victims' claims; and moves political exposure onto protected elites.
% ABSENT_VOICES: Non-party great powers and targeted-state governments object to the broad threshold but hold no vote inside the Assembly's consensus process and communicate mainly through non-cooperation and public attack; accused persons and defense counsel have thin formal voice in admissibility rulings, arriving only after triggers fire; African Union collective criticism enters as bloc non-cooperation rather than agenda access.
% DISAPPEARANCE_RATIONALE: If the broad trigger vanished overnight, impunity in complicit and collapsed states would revert to near-default, victims' organizations would lose their principal escalation venue, targeted elites would shed travel and asset exposure, and the anti-impunity coalition of NGOs, donor states, and Court organs would lose the instrument around which its strategies and budgets are built — diplomacy, tribunal planning, and advocacy pipelines would all reorganize around its absence.
% FOUNDING_PROBLEM: The Nuremberg-legacy problem: when the perpetrator controls the state, domestic prosecution is structurally unavailable, and the post-Cold War atrocities in the former Yugoslavia and Rwanda exposed the absence of a permanent independent forum. Article 17 was drafted to preserve ordinary state primacy while guaranteeing a trigger for exactly the case where state and perpetrator coincide.
% FOUNDING_PROBLEM_CORROBORATION: UN commissions of inquiry (Syria, Darfur, Myanmar), the 2020 ICC Independent Expert Review, and documented impunity outcomes in Ethiopia, Syria, and Myanmar corroborate from outside the beneficiary set that the founding problem persists. Targeted-state governments and African Union communiqués attest the opposite — that domestic mechanisms suffice and the trigger is pretextual — which is counter-attestation from outside the beneficiary set rather than confirmation; the status is corroborated as live by neutral investigative bodies, contested only by the parties the trigger reaches.
narrative_ontology:disappearance_verdict(article_17_complementarity__international_oversight_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__international_oversight_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__international_oversight_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_17_complementarity__international_oversight_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__international_oversight_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__international_oversight_reading_tests).
:- end_tests(article_17_complementarity__international_oversight_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.40: this reading prices the sovereignty transfer and cooperation burdens as the deliberate cost of closing the impunity gap, with most extraction landing on actors the reading deems culpable — but it concedes genuine institutional costs to targeted judiciaries and a widening gap between the trigger's formal breadth and its execution. Suppression 0.58 is authored as a raw structural property (the engine scales only extractiveness by directionality and scope): cooperation demands, referral leverage, asset-freeze reach, and reputational coercion are real, yet short of total because genuine domestic prosecution, Assembly deferral politics, and treaty withdrawal remain available exits. Theater_ratio 0.44 reflects the widening share of declaratory output — unexecuted warrants, long-stalled situations, preliminary examinations maintained as posture — relative to completed trials. Accessibility_collapse 0.48: alternatives only partly collapse; a state that conducts credible proceedings removes the trigger entirely. Resistance 0.62: withdrawals (Burundi, Philippines), non-cooperation campaigns, AU hostility, and sanctions on Court personnel document sustained active resistance. Claim and metrics are independent facts: I claim tangled_rope because the arrangement possesses BOTH a genuine coordination function (impunity-gap closure no other institution performs) AND asymmetric extraction (adjudicative authority and political exposure taken from state seats, received by the Court), sustained by active enforcement — and I author the metrics above from the same structural reading. All three tracked series share one seven-point grid (2002–2024); no metric borrows another's row. The series are monotonic rather than cyclical, with one tension worth flagging: suppression_requirement rises steadily while realized extraction plateaus and declines after 2018 — enforcement infrastructure hardened precisely as cooperation decayed, a ratchet compensating for attrition rather than amplifying effect.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural data. From the complicit executive's position the arrangement is a sword aimed personally at him — high effective extraction, trap-like exit profile, snare-flavored experience. From the victim's position it is a lifeline — subsidy, near-zero extraction, rope-flavored relief. From the Court's own seat it is near-symmetric: it pays heavily in enforcement costs and legitimacy attrition while collecting mandate, docket, and budget. The sibling national-primacy reading is, functionally, another seat's verdict institutionalized — what the payer seats experience, elevated to doctrine. The engine derives this divergence from the structural data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map cleanly: atrocity_victims_in_complicit_states and human_rights_advocacy_network sit at the beneficiary pole (d near 0) — the trigger subsidizes them at others' expense; state_parties_with_functional_judiciaries collect the deterrence externality cheaply. complicit_state_executives sit near the full-target pole (d near 1) — identity-adjacent entrapment: their exposure follows them across borders and survives domestic politics. national_judicial_establishments occupy the middle with a twist — formally payers, materially part-beneficiaries through positive complementarity, which dampens their effective extraction below what a pure-victim declaration would yield. great_power_non_parties are excluded rather than coordinated: the trigger's enforcement object includes keeping their objections outside the Assembly's agenda. Receipt surface: the constraint's extraction (custody, authority, docket) demonstrably accrues to the Court seat, so gain_flow names icc_chambers_and_prosecutor — receipt, not benefit: victims benefit from the arrangement without receiving what is extracted. Fixing cost is prohibitive: unwinding the broad threshold requires statute-amendment supermajorities or appellate self-overturning against an entrenched anti-impunity coalition, far exceeding any single seat's benefit from doing so.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — prosecution structurally unavailable where perpetrator and state coincide — remains live and corroborated by bodies outside the beneficiary set, so no mandatrophy flag is declared and no sunset clause exists to check. The classification's job here is double-sided error prevention: calling this a snare (as sovereignty-first critics effectively do) erases the victim-side subsidy that is the arrangement's entire point and no other institution duplicates; calling it a rope (as the reading's advocates do) erases the real sovereignty transfer, the enforcement asymmetry documented since the Court's first decade, and the growing theatrical share. Tangled_rope holds both faces in one structure and lets the per-seat computations expose the divergence the prose debate keeps flattening. The main drift hazard is not mandate death but capacity death: if warrant-execution rates stay near zero, the guardian function persists as declaratory maintenance — a piton-shaped future inside a tangled-rope present, tracked by the theater_ratio series and the declaratory_deterrence_value omega.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading of the Article 17 kernel governs admissibility practice — this international-oversight reading''s low trigger threshold with burden on the state, or the national-primacy reading''s presumption of domestic adequacy with burden on the Court?',
    'Track appellate jurisprudence on admissibility challenges, Assembly of States Parties amendment activity, and successive Prosecutors'' stated willingness-or-genuineness criteria; a sustained narrowing of unwillingness criteria or a formal burden shift would mark convergence toward the sibling reading.',
    'If the national-primacy reading prevails, the admissibility threshold rises, the burden shifts to demonstrating sham proceedings, the victim set contracts toward proven-sham cases, and the operative constraint becomes the sibling story rather than this one — this story''s epsilon and beneficiary structure describe a constraint that no longer binds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this constraint is the international-oversight reading of the article_17_complementarity kernel; the sibling reading changes threshold, burden allocation, and victim set.').

omega_variable(
    enforcement_capacity_selectivity,
    'Does the broad trigger apply symmetrically across powerful and institutionally weak states, or does intervention capacity concentrate on weaker jurisdictions while great powers and their allies remain effectively beyond reach?',
    'Compare situation selection, cooperation outcomes, and warrant-execution rates by target-state power and alliance profile across successive Prosecutor terms; test whether any situation has proceeded against a great-power ally against that ally''s active opposition.',
    'If extraction concentrates on weak-state payer seats while strong states enjoy de facto immunity, the payer-seat classifications drift toward snare characteristics despite the reading''s universalist premise, and the coordination function acquires a selective-enforcement asterisk that undermines its legitimacy claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_selectivity, empirical, 'Whether enforcement asymmetry contradicts the reading''s equal-application premise.').

omega_variable(
    genuine_intent_assessability,
    'Can ''genuine willingness and ability'' be assessed externally with enough reliability that the trigger reliably separates sham proceedings from imperfect-but-honest domestic efforts?',
    'Audit admissibility outcomes against subsequent domestic-process quality: cases where the Court deferred and proceedings later proved sham, against cases where Court intervention displaced messy but sincere national processes.',
    'Low assessability turns the threshold political rather than legal, raising effective suppression on payer seats and pushing their computed classifications toward snare; high assessability secures the coordination half of the hybrid and stabilizes the tangled-rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_intent_assessability, empirical, 'Reliability of the genuineness standard as the hinge between coordination and extraction.').

omega_variable(
    declaratory_deterrence_value,
    'Do unexecuted warrants, stalled situations, and preliminary examinations produce deterrent or expressive effects that count as functional output, or are they predominantly theatrical maintenance of the guardian claim?',
    'Deterrence studies comparing atrocity incidence in referred versus comparable non-referred contexts, plus longitudinal conversion rates from warrant issuance to custody across situations.',
    'A high theater share would signal inertial drift inside this reading — the guardian function persisting as performance after enforcement capacity atrophied — pulling the constraint''s temporal classification toward piton characteristics at the payer seats even while the formal threshold stays broad.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declaratory_deterrence_value, empirical, 'Whether declaratory activity is functional deterrence or theatrical maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__international_oversight_reading, 2002, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art17_ovsr_tr_t2002, article_17_complementarity__international_oversight_reading, theater_ratio, 2002, 0.18).
narrative_ontology:measurement(art17_ovsr_tr_t2006, article_17_complementarity__international_oversight_reading, theater_ratio, 2006, 0.2).
narrative_ontology:measurement(art17_ovsr_tr_t2010, article_17_complementarity__international_oversight_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(art17_ovsr_tr_t2014, article_17_complementarity__international_oversight_reading, theater_ratio, 2014, 0.33).
narrative_ontology:measurement(art17_ovsr_tr_t2018, article_17_complementarity__international_oversight_reading, theater_ratio, 2018, 0.41).
narrative_ontology:measurement(art17_ovsr_tr_t2022, article_17_complementarity__international_oversight_reading, theater_ratio, 2022, 0.46).
narrative_ontology:measurement(art17_ovsr_tr_t2024, article_17_complementarity__international_oversight_reading, theater_ratio, 2024, 0.44).

% Extraction over time
narrative_ontology:measurement(art17_ovsr_be_t2002, article_17_complementarity__international_oversight_reading, base_extractiveness, 2002, 0.26).
narrative_ontology:measurement(art17_ovsr_be_t2006, article_17_complementarity__international_oversight_reading, base_extractiveness, 2006, 0.31).
narrative_ontology:measurement(art17_ovsr_be_t2010, article_17_complementarity__international_oversight_reading, base_extractiveness, 2010, 0.37).
narrative_ontology:measurement(art17_ovsr_be_t2014, article_17_complementarity__international_oversight_reading, base_extractiveness, 2014, 0.45).
narrative_ontology:measurement(art17_ovsr_be_t2018, article_17_complementarity__international_oversight_reading, base_extractiveness, 2018, 0.47).
narrative_ontology:measurement(art17_ovsr_be_t2022, article_17_complementarity__international_oversight_reading, base_extractiveness, 2022, 0.43).
narrative_ontology:measurement(art17_ovsr_be_t2024, article_17_complementarity__international_oversight_reading, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(art17_ovsr_su_t2002, article_17_complementarity__international_oversight_reading, suppression_requirement, 2002, 0.25).
narrative_ontology:measurement(art17_ovsr_su_t2006, article_17_complementarity__international_oversight_reading, suppression_requirement, 2006, 0.3).
narrative_ontology:measurement(art17_ovsr_su_t2010, article_17_complementarity__international_oversight_reading, suppression_requirement, 2010, 0.38).
narrative_ontology:measurement(art17_ovsr_su_t2014, article_17_complementarity__international_oversight_reading, suppression_requirement, 2014, 0.47).
narrative_ontology:measurement(art17_ovsr_su_t2018, article_17_complementarity__international_oversight_reading, suppression_requirement, 2018, 0.53).
narrative_ontology:measurement(art17_ovsr_su_t2022, article_17_complementarity__international_oversight_reading, suppression_requirement, 2022, 0.56).
narrative_ontology:measurement(art17_ovsr_su_t2024, article_17_complementarity__international_oversight_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__international_oversight_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, article_17_complementarity__national_primacy_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the Article 17 kernel per the epsilon-invariance principle: the colloquial label 'complementarity' covers two structurally distinct constraints. This story instantiates the international-oversight reading (low trigger, burden on the state, ICC as guardian against impunity); the sibling story article_17_complementarity__national_primacy_reading instantiates the sovereignty-protection reading (presumptively adequate national courts, burden on the Court to prove sham). The two readings share the referent — the standing admissibility arrangement — and author different epsilons over it from their respective seats; neither value transfers. Jurisprudence produced under this reading is cited by the sibling as evidence of overreach, and vice versa, so contamination propagates along this edge in both directions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
