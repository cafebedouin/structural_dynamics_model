% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__conditional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__conditional_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__conditional_sovereignty
 *   human_readable: Conditional Sovereignty Doctrine (Responsibility to Protect)
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   The conditional-sovereignty doctrine — sovereignty entails
 *   responsibility; systematic human rights violations trigger legitimate
 *   external intervention — is the reading of the westphalian sovereignty
 *   kernel institutionalized as the Responsibility to Protect: proposed by
 *   the ICISS commission in 2001, adopted in restricted form in the 2005
 *   World Summit Outcome, and operated since through Security Council
 *   practice. The ε referent for this story is the standing
 *   conditional-sovereignty arrangement — the doctrine as adopted and as
 *   actually applied through the Council gate — assessed by this reading's
 *   own lights; it is not the absolute-sovereignty alternative the reading
 *   contests. The doctrine addresses a real collective-action problem (the
 *   sovereignty shield that protected atrocity perpetrators before 2001)
 *   while concentrating threshold determination in the five veto-holding
 *   powers and applying it along intervener-interest lines; it coordinates
 *   and extracts through the same structure. This file is one member of a
 *   constraint family: the absolute_sovereignty and graduated_sovereignty
 *   readings of the same kernel are separate constraints with their own ε
 *   values and stakeholder structures, linked through the network edges
 *   below. KEY AGENTS (by structural relationship): -
 *   western_intervention_coalitions: Primary beneficiary
 *   (institutional/arbitrage) — author and operator of the doctrine; collects
 *   legitimating authority and operational discretion -
 *   targeted_sovereign_states: Primary target (moderate/trapped) — bears the
 *   conditionalization of the sovereignty shield -
 *   blocking_permanent_members: Dual seat (institutional/trapped) —
 *   agenda_setters by veto who bear the doctrine as standing exposure and pay
 *   diplomatic capital to hold the gate closed - atrisk_civilian_populations:
 *   Declared beneficiary, procedurally excluded (powerless/trapped) —
 *   protection is decided in their name without their consent -
 *   un_security_council: Agenda-setter institution (institutional/trapped) —
 *   the gate through which all threshold determinations pass -
 *   human_rights_advocacy_organizations: Secondary beneficiary
 *   (organized/mobile) — mandate, funding, and standing flow from the
 *   doctrine - african_union_regional_bodies: Dual-positioned adopter
 *   (organized/constrained) — embraced regional conditionality while
 *   resisting external conditionality - international_law_scholars:
 *   Analytical observer (analytical/analytical) — sees the full structure,
 *   collects nothing
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__conditional_sovereignty, 0.44).
domain_priors:suppression_score(westphalian_sovereignty__conditional_sovereignty, 0.58).
domain_priors:theater_ratio(westphalian_sovereignty__conditional_sovereignty, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, extractiveness, 0.44).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__conditional_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__conditional_sovereignty, "Conditional Sovereignty Doctrine (Responsibility to Protect)").
narrative_ontology:topic_domain(westphalian_sovereignty__conditional_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__conditional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__conditional_sovereignty, '1dc86d96-41d4-4b2a-b0f1-b9e8928e3f55').
narrative_ontology:cs_kernel_codification('1dc86d96-41d4-4b2a-b0f1-b9e8928e3f55', fixed_text).
narrative_ontology:cs_authority_grounding('1dc86d96-41d4-4b2a-b0f1-b9e8928e3f55', extraction).
narrative_ontology:cs_interpretation_layer_present('1dc86d96-41d4-4b2a-b0f1-b9e8928e3f55').
narrative_ontology:cs_reading_relation('1dc86d96-41d4-4b2a-b0f1-b9e8928e3f55', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('1dc86d96-41d4-4b2a-b0f1-b9e8928e3f55', westphalian_sovereignty__graduated_sovereignty, influences).
narrative_ontology:cs_axiom('1dc86d96-41d4-4b2a-b0f1-b9e8928e3f55', foundational, sovereignty_entails_responsibility_for_populations).
narrative_ontology:cs_axiom_status(sovereignty_entails_responsibility_for_populations, holdable).
narrative_ontology:cs_axiom_grounding('1dc86d96-41d4-4b2a-b0f1-b9e8928e3f55', sovereignty_entails_responsibility_for_populations, deontological).
narrative_ontology:cs_axiom('1dc86d96-41d4-4b2a-b0f1-b9e8928e3f55', foundational, systematic_atrocity_violations_forfeit_nonintervention_shield).
narrative_ontology:cs_axiom_status(systematic_atrocity_violations_forfeit_nonintervention_shield, holdable).
narrative_ontology:cs_axiom_grounding('1dc86d96-41d4-4b2a-b0f1-b9e8928e3f55', systematic_atrocity_violations_forfeit_nonintervention_shield, instrumental).
narrative_ontology:cs_reference_frame('1dc86d96-41d4-4b2a-b0f1-b9e8928e3f55', sovereignty_as_responsibility_norm).
narrative_ontology:cs_drift_state('1dc86d96-41d4-4b2a-b0f1-b9e8928e3f55', post_libya_mandate_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1dc86d96-41d4-4b2a-b0f1-b9e8928e3f55', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, western_intervention_coalitions).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, human_rights_advocacy_organizations).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, atrisk_civilian_populations).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, targeted_sovereign_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, blocking_permanent_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, african_union_regional_bodies).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, african_union_regional_bodies).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, sovereignty_as_responsibility_doctrine).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, responsibility_to_protect_norm).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, humanitarian_intervention_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States and alliances with global power-projection capacity — the permanent Western members of the Security Council and their coalition partners. They author and invoke the doctrine, draft the threshold language, and lead the coalitions that execute authorized interventions. The doctrine gives their operations a shared legitimating vocabulary and lowers the diplomatic cost of acting; they can also act outside it when the gate blocks them, as Kosovo showed. Their capacity does not depend on the doctrine, so exit from it is easy.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, western_intervention_coalitions, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, western_intervention_coalitions, agenda_setter).

% The non-Western permanent members of the Security Council. They hold the same veto as the intervention coalitions and spend it to block threshold findings that would expose their own conduct or their partners'. They bear the doctrine as a standing exposure — its standards are ones their internal conduct can be measured against — and they pay in diplomatic capital to hold the gate closed. They cannot leave the Council without forfeiting the veto that protects them.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, blocking_permanent_members, agenda_setter,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, blocking_permanent_members, payer).

% The institutional gate through which every threshold determination and authorization must pass under the doctrine as adopted. Its veto structure decides which violations trigger a response and which do not; its practice since 2005 — authorizing in Libya, blocking in Syria — is the doctrine's operative content. The institution persists regardless of which governments occupy its seats.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, un_security_council, agenda_setter,
    institutional, generational, trapped, global).

% States whose internal conduct becomes adjudicable once the doctrine's thresholds are argued to be met. They are typically states with limited material power to resist a finding or a coalition; the sovereignty shield the pre-2001 norm guaranteed them is now conditional on a determination they do not control. They cannot exit the international legal order, and renouncing territorial sovereignty is not an available option.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, targeted_sovereign_states, payer,
    moderate, generational, trapped, national).

% Civilian populations facing or fleeing atrocity crimes — the people in whose name the doctrine proceeds. When intervention comes they may be protected; when it is blocked or misfires they bear the costs either of abandonment or of the intervention itself. They hold no procedural seat in the determination process: their consent is never solicited, and interveners and their governments speak for them.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, atrisk_civilian_populations, beneficiary,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, atrisk_civilian_populations, excluded).

% Transnational NGOs, commissions of inquiry, and atrocity-prevention bodies. They document violations, press threshold findings, and staff the doctrine's institutional infrastructure. The doctrine supplies their mandate, funding streams, and standing; their advocacy is portable across cases and causes, so their attachment to this particular framework is a matter of portfolio, not survival.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, human_rights_advocacy_organizations, beneficiary,
    organized, biographical, mobile, global).

% The African Union and sub-regional bodies that wrote the responsibility-to-protect idea into their own constitutive instruments — the AU's Article 4(h) authorizes regional intervention for atrocity crimes. They gain a regional doctrine of non-indifference that they control; their member states are simultaneously the population most exposed to the doctrine's external application, so the same organizations defend regional conditionality while resisting external conditionality.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, african_union_regional_bodies, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, african_union_regional_bodies, payer).

% International lawyers, just-war theorists, and political philosophers who produce the doctrine's interpretive literature — its coherence critiques, its threshold jurisprudence, its genealogies from Grotius to the ICISS commission. They observe and analyze; they collect nothing from the doctrine's operation and bear none of its costs.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__conditional_sovereignty, western_intervention_coalitions).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__conditional_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem the absolute sovereignty norm created: atrocities committed inside sovereign borders generated no legitimate response, because the non-intervention rule shielded perpetrators and unilateral intervention was free-riding. The doctrine supplies a shared threshold — systematic atrocity crimes — at which the shield yields, so that response is coordinated rather than discretionary, and gives regional organizations a template for their own non-indifference doctrines.
% TRANSFER_FUNCTION: Moves decision authority over internal affairs from targeted states to the Security Council and intervention-capable coalitions when thresholds are found met; moves reputational and legal standing from every state (now a duty-bearer whose sovereignty must be justified as responsible conduct) toward the doctrine's interpreters and gatekeepers; moves the material costs of intervention onto targeted territories and their populations.
% ABSENT_VOICES: The populations of targeted states — in whose name the doctrine proceeds — hold no procedural seat in threshold determination; their consent is neither solicited nor required, and interveners speak for them. The non-interventionist legal tradition of much of the Global South is voiced in General Assembly debate but excluded from the Council process where the doctrine actually operates. Both would object to the arrangement's current terms if seated where determinations are made.
% DISAPPEARANCE_RATIONALE: Intervention-capable powers intervened before the doctrine existed (Kosovo, 1999) and would presumably act under other pretexts where interests align, so the raw practice of powerful states entering weak ones might persist largely unchanged. But the shared threshold framework, the AU's regional adoption, the prevention infrastructure, and the duty-bearing status of all states would dissolve, returning threshold questions to unmediated power politics. Whether that rearranges the world or merely relabels existing practice is precisely what the parties dispute — the intervention coalitions say the framework disciplines power; the targeted and blocking states say it launders it.
% FOUNDING_PROBLEM: The atrocity failures of the 1990s: Rwanda (1994) and Srebrenica (1995) exposed the sovereignty shield as impunity for mass atrocity; Kosovo (1999) exposed unilateral intervention as widely tolerated but formally illegitimate. Kofi Annan's challenge to the General Assembly — how to reconcile state sovereignty, in its Charter meaning, with the international community's inability to prevent atrocities — produced the ICISS commission and this doctrine.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: the African Union's peace-and-security organs (which adopted the regional variant for their own use), General Assembly atrocity-prevention dialogues initiated by non-Western member states, and the empirical record of mass-atrocity events 2011-2025 (Syria, Ethiopia, Myanmar, Sudan) corroborate that the founding problem persists. The seats dispute only whether this doctrine is the right answer to it — the problem's liveness is not contested by any seat.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__conditional_sovereignty, contested).
narrative_ontology:founding_problem_status(westphalian_sovereignty__conditional_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__conditional_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalian_sovereignty__conditional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__conditional_sovereignty, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__conditional_sovereignty_tests).
:- end_tests(westphalian_sovereignty__conditional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claim is tangled_rope: the doctrine coordinates a real collective-action problem — before 2001 the absolute sovereignty norm gave atrocity perpetrators an impunity shield, and the international community had no shared framework for when it yields; Security Council Resolution 1973 (2011) and the AU's Article 4(h) show the coordination function delivering rather than merely promised. The same structure extracts asymmetrically: threshold determination sits behind the P5 veto, application tracks intervener interest (authorized in Libya, blocked in Syria), and targeted states cannot exit the legal order whose shield has been conditionalized. Both the coordination function and the enforcement machinery are real — the tangled-rope signature. The manifest hypothesis of snare is recorded in uke_scope and left unreconciled; my analysis refines it. Metrics: extractiveness 0.44 is moderate — the extraction is threshold-gated rather than constant, but the standing vulnerability applies to every state and its severity when triggered is high. Suppression 0.58 measures the doctrine's raw foreclosing force: the absolute-sovereignty defense is delegitimized in Council argument and states cannot opt out of duty-bearing status; suppression is a structural property and is deliberately unscaled — only extractiveness is scaled by directionality and scope downstream. Theater 0.50: the annual dialogues, reports, and focal-point networks are substantial ritual, and the invocation-without-action pattern (Darfur, Syria) is increasingly performative, but the prevention infrastructure and the Council's authorization practice remain functional. Accessibility collapse 0.35: the alternative readings remain live and accessible — absolute sovereignty is still asserted by major powers and graduated proposals circulate; the kernel contest is open. Resistance 0.65: organized and sustained — G77 and Non-Aligned objections, BRIC pushback after Libya, Brazil's responsibility-while-protecting proposal, recurring vetoes. The measurement series run on one shared grid (t=0 is 2001, the ICISS report; t=24 is 2025; points fall at 2005, 2009, 2013, 2017, 2021) so every tracked metric is authored at every examined point; the theater series dips slightly after 2021 as Ukraine-era cynicism of invocations led some actors to abandon the vocabulary entirely rather than perform it. Receipt surface: the extraction's gains demonstrably accrue to the intervention coalitions (legitimating authority plus operational discretion), and fixing the asymmetry is prohibitive because repairing threshold determination requires Security Council reform, which the veto structurally blocks relative to the benefit any single fixer could capture.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the intervention coalitions' seat the doctrine is an instrument they author and can route around — a legitimating asset at negligible cost, computing as coordination or subsidy. From the targeted states' seat the same structure is a conditionalized shield: their autonomy is now contingent on a determination they do not control, computing as enforced extraction. The blocking permanent members hold the sharpest internal split — they operate the gate (agenda_setter) while being exposed by the standard it guards (payer), so their seat mixes gatekeeper benefit with target cost. The at-risk populations' seat is the doctrine's constitutive paradox: they are its declared purpose and its procedural absentees, experiencing promise, abandonment, and intervention's costs in sequence. The AU bodies hold a genuine dual position — the same conditionality they adopted regionally is the one they resist externally — so their seat should not resolve to either pure pole.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (intervention coalitions, advocacy organizations, at-risk populations) derive low directionality for those seats; the victim declarations (targeted states, blocking members) derive high directionality. Exit structure modulates within that: the coalitions' arbitrage-grade exit (Kosovo showed they act without the doctrine when the gate blocks them) sits them nearest the beneficiary end; the targeted states' trapped exit (no exit from the international legal order; territorial sovereignty is not renounceable) sits them nearest the full-target end; the at-risk populations are trapped and powerless — they cannot coalition their way out because they are the object of the determination, not parties to it. Two seats resist clean derivation and are left to the structural data rather than overrides (overrides key on power atom and would contaminate the other institutional seats): the blocking permanent members are declared victims but their veto insulates them from operational application — their extraction is exposure and defensive expenditure rather than intervention; and the AU bodies collect a regional doctrine they control while their members bear the external variant. The scholars' seat is analytical and collects nothing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — atrocity impunity behind the sovereignty shield — is live and externally corroborated (AU peace-and-security practice, General Assembly atrocity-prevention dialogues initiated by non-Western states, the empirical record of mass atrocities 2011-2025), so this is not a mandatrophy case: the mandate has not outlived its function. The classification prevents mislabeling in both directions. It prevents the pure-coordination mislabel: the extraction asymmetry is not coordination overhead — the P5 gate and interest-tracked application concentrate the doctrine's costs on states with the least capacity to resist, which no coordination function requires. It prevents the pure-extraction mislabel: the coordination function is not cover — it was adopted by post-colonial states for their own regional use (AU Article 4(h)), delivered a coordinated authorization in Libya, and built prevention infrastructure that persists. The mandatrophy-adjacent question is whether the third pillar's atrophy (the intervention trigger becoming a dead letter post-Libya) is degrading the constraint toward inertia — the pillar_three_dead_letter omega tracks exactly that, and the theater trajectory is the observable that would confirm it. If the trigger dies while the dialogues continue, the doctrine becomes performance around a hollowed mandate; the current data (theater 0.50, trigger still formally live) place it short of that line.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading — conditional_sovereignty — of the westphalian_sovereignty kernel. Which reading governs the standing arrangement, and how would the sibling readings (absolute_sovereignty, graduated_sovereignty) change the constraint''s structure?',
    'Observe which reading operative bodies actually apply in threshold disputes: Security Council determinations, General Assembly debate outcomes, and regional-organization practice reveal the governing reading without further authoring.',
    'Under the absolute reading, external interference is categorically illegitimate and this constraint''s beneficiary/victim structure inverts; under the graduated reading, conditionality is indexed to state capacity and governance legitimacy rather than atrocity conduct, which changes the victim set from conduct-targeted states to low-capacity states generally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the westphalian sovereignty kernel the operative arrangement instantiates.').

omega_variable(
    threshold_determination_capture,
    'Is the determination of systematic human rights violations — the doctrine''s trigger — structurally captured by the intervention-capable powers through the Security Council gate?',
    'Comparative case analysis of threshold findings (Libya, Syria, Yemen, Ethiopia, Myanmar) correlated against P5 interests and veto patterns; if findings track intervener interest rather than violation severity, capture is established.',
    'If captured, the doctrine''s extraction is more asymmetric than the base measure indicates and its coordination function is gate-kept — pushing the structure toward pure extraction; if findings track severity, the coordination function dominates and the hybrid reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_determination_capture, empirical, 'Whether the intervention trigger is determined by violation severity or by intervener interest.').

omega_variable(
    atrisk_benefit_delivery,
    'Do at-risk civilian populations — the doctrine''s declared beneficiaries — actually capture its promised protection, or do they bear intervention''s costs when it is authorized and abandonment''s when it is not?',
    'Post-intervention civilian-protection outcomes against counterfactual baselines (Libya post-2011 is the sharpest test), plus protection outcomes in blocked cases (Syria, Myanmar).',
    'If populations systematically fail to benefit, the declared beneficiary structure is misdescribed and the doctrine operates as extraction with humanitarian cover; if protection is delivered in authorized cases, the coordination benefit is real and the hybrid classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrisk_benefit_delivery, empirical, 'Whether the doctrine''s declared beneficiaries receive its promised benefit.').

omega_variable(
    pillar_three_dead_letter,
    'Has the doctrine''s coercive third pillar — the external intervention trigger — atrophied into a dead letter since the Libya mandate controversy, leaving only the non-coercive first two pillars operative?',
    'Count of Chapter VII authorizations invoking protection-of-civilians language before and after 2011, and the fate of subsequent threshold claims (the Syria vetoes of 2012-2013; no protection-of-civilians authorization since).',
    'If the trigger is dead, the constraint''s operative content narrows to state duty-bearing and assistance — lower extraction, higher theater — and the standing vulnerability of targeted states is largely notional; if the trigger remains live, the full threshold-gated extraction stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pillar_three_dead_letter, empirical, 'Whether the intervention trigger remains operative or has atrophied post-Libya.').

omega_variable(
    selectivity_structural_or_contingent,
    'Is the doctrine''s selective application — authorized in some atrocity cases, blocked in others along intervener-interest lines — a structural feature of the arrangement or a contingent artifact of current Security Council politics?',
    'Natural experiment across institutional variations: if Uniting-for-Peace pathways, regional authorizations (ECOWAS, AU Article 4(h)), or any Security Council reform ever produce severity-tracked application, the selectivity is contingent; persistence of interest-tracked application across institutional variations indicates structure.',
    'If structural, the doctrine is an instrument of power projection wearing coordination clothing and its extraction concentrates on the powerless; if contingent, the asymmetry could be reformed without abandoning the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_structural_or_contingent, empirical, 'Whether asymmetric application is built into the arrangement or an artifact of current politics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__conditional_sovereignty, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(westphalian_conditional_sov_tr_t0, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(westphalian_conditional_sov_tr_t0, observed).
narrative_ontology:measurement(westphalian_conditional_sov_tr_t4, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 4, 0.31).
narrative_ontology:measurement_basis(westphalian_conditional_sov_tr_t4, observed).
narrative_ontology:measurement(westphalian_conditional_sov_tr_t8, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 8, 0.38).
narrative_ontology:measurement_basis(westphalian_conditional_sov_tr_t8, observed).
narrative_ontology:measurement(westphalian_conditional_sov_tr_t12, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 12, 0.45).
narrative_ontology:measurement_basis(westphalian_conditional_sov_tr_t12, observed).
narrative_ontology:measurement(westphalian_conditional_sov_tr_t16, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 16, 0.48).
narrative_ontology:measurement_basis(westphalian_conditional_sov_tr_t16, observed).
narrative_ontology:measurement(westphalian_conditional_sov_tr_t20, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 20, 0.52).
narrative_ontology:measurement_basis(westphalian_conditional_sov_tr_t20, observed).
narrative_ontology:measurement(westphalian_conditional_sov_tr_t24, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 24, 0.5).
narrative_ontology:measurement_basis(westphalian_conditional_sov_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(westphalian_conditional_sov_be_t0, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(westphalian_conditional_sov_be_t0, observed).
narrative_ontology:measurement(westphalian_conditional_sov_be_t4, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 4, 0.34).
narrative_ontology:measurement_basis(westphalian_conditional_sov_be_t4, observed).
narrative_ontology:measurement(westphalian_conditional_sov_be_t8, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 8, 0.38).
narrative_ontology:measurement_basis(westphalian_conditional_sov_be_t8, observed).
narrative_ontology:measurement(westphalian_conditional_sov_be_t12, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 12, 0.41).
narrative_ontology:measurement_basis(westphalian_conditional_sov_be_t12, observed).
narrative_ontology:measurement(westphalian_conditional_sov_be_t16, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 16, 0.43).
narrative_ontology:measurement_basis(westphalian_conditional_sov_be_t16, observed).
narrative_ontology:measurement(westphalian_conditional_sov_be_t20, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 20, 0.44).
narrative_ontology:measurement_basis(westphalian_conditional_sov_be_t20, observed).
narrative_ontology:measurement(westphalian_conditional_sov_be_t24, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 24, 0.44).
narrative_ontology:measurement_basis(westphalian_conditional_sov_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(westphalian_conditional_sov_su_t0, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(westphalian_conditional_sov_su_t0, observed).
narrative_ontology:measurement(westphalian_conditional_sov_su_t4, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 4, 0.48).
narrative_ontology:measurement_basis(westphalian_conditional_sov_su_t4, observed).
narrative_ontology:measurement(westphalian_conditional_sov_su_t8, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 8, 0.52).
narrative_ontology:measurement_basis(westphalian_conditional_sov_su_t8, observed).
narrative_ontology:measurement(westphalian_conditional_sov_su_t12, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 12, 0.55).
narrative_ontology:measurement_basis(westphalian_conditional_sov_su_t12, observed).
narrative_ontology:measurement(westphalian_conditional_sov_su_t16, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 16, 0.56).
narrative_ontology:measurement_basis(westphalian_conditional_sov_su_t16, observed).
narrative_ontology:measurement(westphalian_conditional_sov_su_t20, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 20, 0.57).
narrative_ontology:measurement_basis(westphalian_conditional_sov_su_t20, observed).
narrative_ontology:measurement(westphalian_conditional_sov_su_t24, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 24, 0.58).
narrative_ontology:measurement_basis(westphalian_conditional_sov_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__conditional_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__graduated_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, un_charter_article_2_7_nonintervention).

% DUAL FORMULATION NOTE:
% Constraint family: the westphalian_sovereignty kernel decomposes into three readings per the ε-invariance principle — absolute_sovereignty (interference categorically illegitimate; its own beneficiary and victim structure), conditional_sovereignty (this file: moderate threshold-gated extraction with a real coordination function), and graduated_sovereignty (capacity-indexed conditionality; a different victim set — low-capacity states generally rather than conduct-targeted states). The readings are linked, not merged: the absolute reading is the historical baseline the conditional reading modified, and the graduated reading is the reform proposal the conditional reading's threshold fights made thinkable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
