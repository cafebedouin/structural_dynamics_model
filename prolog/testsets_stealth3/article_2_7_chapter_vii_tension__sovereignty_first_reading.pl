% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__sovereignty_first_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__sovereignty_first_reading, []).

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
 *   constraint_id: article_2_7_chapter_vii_tension__sovereignty_first_reading
 *   human_readable: Non-Intervention Barrier: Sovereignty-First Reading of the Article 2(7)/Chapter VII Kernel
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   This story instantiates the sovereignty_first_reading of the
 *   article_2_7_chapter_vii_tension kernel: the standing arrangement under
 *   contest is the non-intervention barrier itself — the Charter order in
 *   which Article 2(7) insulates domestic jurisdiction and Chapter VII opens
 *   the only lawful breach, gated by five concurrences and construed as
 *   reaching inter-state aggression rather than internal atrocity. The
 *   barrier genuinely solves the largest collective-action problem in modern
 *   history (mutual forbearance among states, near-elimination of interstate
 *   war among its adherents, survival guarantee for post-colonial states),
 *   and it simultaneously transfers the mortal risk of domestic atrocity onto
 *   the populations who cannot summon outside help. Epsilon's referent is
 *   fixed to that standing arrangement's operation; the value is indexed to
 *   this reading's own lights, which see the abandonment cost plainly — the
 *   English School pluralists and the quasi-states literature wrote it down —
 *   and accept it knowingly as the price of interstate order. The colloquial
 *   label 'Article 2(7)/Chapter VII tension' fuses two structurally distinct
 *   constraints; the sibling r2p_reading (separate file, linked in network)
 *   instantiates the protection-duty constraint with its own epsilon,
 *   victims, and type. Claim and metrics are authored independently: the
 *   claimed type records what this reading believes is structurally true of
 *   the barrier; the metrics record what descriptively happens.
 *
 * KEY AGENTS:
 *   - - permanent_five_members: Agenda-setter and secondary beneficiary (institutional/arbitrage) — jointly administer the only lawful gate through the barrier; each veto shields clients and self while the authorization option is retained for aligned interests
 *   - - postcolonial_state_governments: Primary beneficiary (organized/identity_locked) — collect the anti-predation guarantee that constitutes their post-1945/1960s statehood
 *   - - authoritarian_regime_leaderships: Concentrated beneficiary (powerful/trapped) — convert the barrier into personal impunity, scaled to a Council patron
 *   - - populations_under_domestic_atrocity: Primary target (powerless/trapped) — bear the barrier's mortal cost; their last-resort external option is foreclosed by the same gate their attackers influence
 *   - - persecuted_internal_minorities: Secondary target (powerless/trapped) — bear the slow-motion cost across generations, watching protective doctrines announced and never applied
 *   - - r2p_advocacy_coalitions: Excluded voice (organized/constrained) — the organized objection, present as petitioners without a vote in any chamber that maintains the barrier
 *   - - comparative_international_lawyers: Analytical observer (analytical/analytical) — maps the doctrine, its exceptions, and its body count; adjudicates nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.76).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.64).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__sovereignty_first_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__sovereignty_first_reading, "Non-Intervention Barrier: Sovereignty-First Reading of the Article 2(7)/Chapter VII Kernel").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__sovereignty_first_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__sovereignty_first_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'fa6e9ebf-65c8-4469-ac15-1ea0ff720b7e').
narrative_ontology:cs_kernel_codification('fa6e9ebf-65c8-4469-ac15-1ea0ff720b7e', fixed_text).
narrative_ontology:cs_authority_grounding('fa6e9ebf-65c8-4469-ac15-1ea0ff720b7e', lineage).
narrative_ontology:cs_interpretation_layer_present('fa6e9ebf-65c8-4469-ac15-1ea0ff720b7e').
narrative_ontology:cs_reading_relation('fa6e9ebf-65c8-4469-ac15-1ea0ff720b7e', article_2_7_chapter_vii_tension__r2p_reading, coexists_with).
narrative_ontology:cs_axiom('fa6e9ebf-65c8-4469-ac15-1ea0ff720b7e', foundational, sovereign_equality_unconditional).
narrative_ontology:cs_axiom_status(sovereign_equality_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('fa6e9ebf-65c8-4469-ac15-1ea0ff720b7e', sovereign_equality_unconditional, deontological).
narrative_ontology:cs_axiom('fa6e9ebf-65c8-4469-ac15-1ea0ff720b7e', secondary, chapter_vii_interstate_aggression_scope).
narrative_ontology:cs_axiom_status(chapter_vii_interstate_aggression_scope, holdable).
narrative_ontology:cs_axiom_grounding('fa6e9ebf-65c8-4469-ac15-1ea0ff720b7e', chapter_vii_interstate_aggression_scope, conventional).
narrative_ontology:cs_reference_frame('fa6e9ebf-65c8-4469-ac15-1ea0ff720b7e', westphalian_sovereign_equality_frame).
narrative_ontology:cs_drift_state('fa6e9ebf-65c8-4469-ac15-1ea0ff720b7e', contemporary_post_2005_summit_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('fa6e9ebf-65c8-4469-ac15-1ea0ff720b7e', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, postcolonial_state_governments).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regime_leaderships).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, permanent_five_members).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, persecuted_internal_minorities).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__sovereignty_first_reading, westphalian_non_intervention_doctrine).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__sovereignty_first_reading, charter_sovereign_equality_principle).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__sovereignty_first_reading, p5_concert_gatekeeping_of_force).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five permanent Security Council members jointly run the only lawful gate through the non-intervention rule. Each holds a veto it uses to shield clients and itself, and each retains the option to assemble an authorized coalition when its interests align (Kuwait 1990, Libya 2011) or to breach the rule extra-legally when they do not (Kosovo 1999, Iraq 2003) and absorb the legitimacy cost. The gate's selectivity is not a malfunction of their administration; it is the administration. Their exit position is arbitrage: they move between rule and exception as interest dictates, bearing only reputation costs.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, permanent_five_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__sovereignty_first_reading, permanent_five_members, beneficiary).

% Governments of formerly colonized states treat the non-intervention rule as the principal legal guarantee of their existence: it forecloses recolonization by force, gunboat-backed conditionality, and predation by regional hegemons. They defend Article 2(7) in every United Nations forum, sponsor declaratory resolutions reaffirming it, and read every protective-conditionality proposal as neo-colonialism. Leaving the arrangement would mean accepting that their statehood is conditional on domestic conduct — an existential concession, since sovereign equality is constitutive of what they became in the decolonization wave. They act collectively through the Group of 77 and the Non-Aligned Movement.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, postcolonial_state_governments, beneficiary,
    organized, generational, identity_locked, global).

% Leaderships that rule by repression convert the rule into personal impunity: conduct inside their borders is, under the reading's legal logic, insulated from external correction short of cross-border aggression. They cultivate a patron on the Security Council to guarantee the veto, and their exposure to sanction, arrest, or removal scales directly with any weakening of the rule. They cannot exit: accepting protective-intervention doctrines means accepting their own indictability, so they hold the arrangement with everything they have.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regime_leaderships, beneficiary,
    powerful, biographical, trapped, national).

% Civilians facing systematic massacre, siege, or engineered famine inside a state the rule shields. Their last-resort option — external protective force — is foreclosed unless five governments concur, and the government attacking them holds or can borrow a veto. Flight means refugee camps or neighboring states bound by the same forbearance logic; staying means the mercy of the government assaulting them. Nothing in the arrangement gives them a voice, a vote, or an exit; their protection depends entirely on the goodwill of the authorities harming them.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity, payer,
    powerless, immediate, trapped, national).

% Minority communities living under slow-motion persecution — cultural erasure, mass detention, exclusion structured into law — inside shielded states. Their horizon differs from acute-atrocity populations: they bargain with the arrangement across decades, watching protective doctrines announced at summits and never applied to their case. Emigration offers partial escape at the cost of community existence; the arrangement follows the community that stays, generation after generation.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, persecuted_internal_minorities, payer,
    powerless, generational, trapped, regional).

% Atrocity-prevention organizations, human-rights movements, and supportive middle-power governments press for a protective-intervention duty. They hold no vote in the Security Council, their access runs through the very states the rule shields, and their proposals die in procedural channels the permanent five control. They are the organized voice of the rule's casualties: present as petitioners, absent as principals, unable to leave a system whose only decision points are closed to them.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, r2p_advocacy_coalitions, excluded,
    organized, generational, constrained, global).

% Scholars and jurists who map the rule's doctrine, its exceptions, and its human ledger across cases from Congo to Rwanda to Syria. They adjudicate nothing, collect nothing, and pay nothing; their seat is the analytical record of what the arrangement protects and what it abandons, and the only influence they exert is citation.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, comparative_international_lawyers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regime_leaderships).
narrative_ontology:fixing_cost_class(article_2_7_chapter_vii_tension__sovereignty_first_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a universal rule of mutual non-interference among sovereign states: each state renounces intervention in others' domestic affairs in exchange for identical forbearance toward itself, closing the intervention license that produced centuries of religious war and colonial conquest, and giving weak and newly independent states a legal shield against predation by the strong.
% TRANSFER_FUNCTION: Moves security-of-position from populations inside states to the governments that hold them: the rule converts would-be protective intervention into unlawful interference, transferring impunity to governing elites and transferring the mortal risk of domestic atrocity onto the populations who cannot summon outside help.
% ABSENT_VOICES: Populations under attack have no seat anywhere in the arrangement: the Security Council speaks for states, the General Assembly votes by governments, and the victims of the conduct the rule shields appear only as agenda items sponsored by others. Advocacy coalitions attend as petitioners without votes. The people whose lives the arrangement trades away are constitutionally absent from every chamber where it is maintained.
% DISAPPEARANCE_RATIONALE: If the barrier vanished overnight, the interstate order would rearrange violently: great powers would assert unilateral intervention rights, weak and post-colonial states would scramble for great-power patronage or nuclear hedging, regional hegemons would normalize cross-border correction, and the reciprocity equilibrium that suppressed interstate war among adherents would unwind — while, simultaneously, protective intervention into atrocity situations would become lawful by default and the shielded-perpetrator class would lose its impunity within a decade.
% FOUNDING_PROBLEM: Build a peace order among states that restrains unilateral intervention by the strong — the pre-1945 license under which religious wars and colonial conquest proceeded — while preserving a collective-security channel against cross-border aggression, and extend that guarantee to the decolonizing world whose statehood was otherwise hostage to former masters and regional giants.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: diplomatic historians of the pre-Charter intervention record, the revealed preference of dozens of small states with no extraction interest that adhere to the rule at real cost, and English School and legal-formalist scholarship that defends the norm while explicitly documenting its abandonment trade-off. No serious corroborator denies the atrocity-abandonment cost; they attest the founding problem's continued liveness, not the arrangement's sufficiency.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__sovereignty_first_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__sovereignty_first_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.76 at interval end) because the barrier's cost side is denominated in unaverted atrocity deaths while its benefit side accrues as regime security to the governments the barrier shields; the gap widened as intervention capacity grew after the Cold War, making each blocked protection more costly. Suppression (0.64, unscaled raw structural property) reflects the active delegitimation of alternatives — unilateral humanitarian intervention and applied R2P are priced in veto risk and neo-imperialism charges — plus the total absence of exit for the populations inside shielded states. Theater (0.46) tracks the growth of declaratory machinery: General Assembly resolutions, Human Rights Council inquiries, commissions of report-writers whose output substitutes for deed; it approaches but does not cross the proxy-drift threshold because the barrier's blocking function remains fully real. Accessibility collapse is moderate (0.58): alternatives do not vanish (Kosovo 1999 happened; coalitions assembled) but each attempt pays heavy legitimacy cost. Resistance (0.60) is organized and persistent yet structurally outgunned. Enforcement history is cyclical, not monotonic: installation and declaratory entrenchment (1945-1975), Cold War veto-freeze, post-Cold War challenge wave peaking at the 2005 Summit defense (suppression_requirement 0.74), then re-entrenchment after Libya 2011 as challengers demoralized (falling to 0.64). The cycle itself functions partly as an extraction mechanism: each challenge wave extracts a concessive language concession (R2P 2005) that is absorbed rhetorically without operational change — intermittent concession that launders the barrier's legitimacy while leaving its operation intact. All three series share one nine-point grid. Coordination-type note: declaring identity_coordination accommodates genuine boundary-maintenance complexity; it does not excuse the Power-x-Scope coupling that concentrates this constraint's costs on powerless agents at national scope while its administrators hold global arbitrage.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the postcolonial-government seat the barrier is the constitution of the society of states — the arrangement that made decolonization irreversible — and its holder is identity_locked: sovereignty is not a policy these governments hold but what they are, fused institutionally since 1945-1960; break the identity frame (accept conditionality) and the seat's entire classification inverts. From the atrocity-population seat the same structure computes as abandonment with a legal alibi: trapped, powerless, no coalition path — victims are dispersed across jurisdictions and are targeted precisely because organizing internally is lethal, while international solidarity channels run through the very states the barrier shields. From the P5 seat the barrier is a discretionary instrument: administered, collected from, and arbitraged around at will. The engine derives these per-seat classifications from the authored power/exit/role data; this commentary does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Postcolonial_state_governments and authoritarian_regime_leaderships sit near the beneficiary pole (low d): the barrier subsidizes them with forbearance and impunity respectively, and neither can exit without existential loss — identity lock for the former, indictment exposure for the latter. Populations_under_domestic_atrocity and persecuted_internal_minorities sit at the target pole (d near 1.0): they receive the barrier's full cost with zero offsetting flow, and trapped exit amplifies their effective extraction. Permanent_five_members derive mixed directionality: they administer the gate (agenda_setter), collect secondarily (client-shielding, selective authorization rents), and hold arbitrage-grade exit — they can breach or waive the barrier when convenient — which pulls their d toward the middle-low range rather than the pure-beneficiary end. Scope amplification applies modestly at the barrier's global reach; suppression enters the computation unscaled, as authored.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — restraining unilateral intervention by the strong while preserving a collective channel against cross-border aggression — remains live: great-power revisionism and regional hegemonism are current facts, not historical memories. The mandate has not outlived its function, so no mandatrophy declaration is authored, and the R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges: no zombie flag. The classification guards against two mislabels. Against pure-snare: the coordination function is genuine and enormous — the barrier is plausibly the single largest contributor to the post-1945 decline in interstate war, and its post-colonial beneficiaries are not cover-story fiction. Against pure-rope: the victim set is real, identifiable, and mortally burdened, and the extraction is asymmetric by design rather than incident. Theater at 0.46 keeps the constraint short of piton drift: the blocking function still works, and the declaratory surplus, while growing, has not replaced it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_delta_vs_r2p,
    'This constraint is the sovereignty_first_reading of the article_2_7_chapter_vii_tension kernel; what structurally changes if the sibling r2p_reading is taken as operative on the same kernel instead?',
    'Read the sibling story''s authored epsilon, victim set, and type: the R2P instantiation relocates the trigger (systematic atrocity summons external responsibility), shrinks the victim set to Council-refusal cases, and redistributes beneficiary position toward protected populations.',
    'Per-seat classifications invert for the atrocity-population seats: the same populations compute as targets of the barrier under this reading and as holders of an enforceable claim under the sibling. The shared kernel carries two incompatible operative constraints, not one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_delta_vs_r2p, conceptual, 'Committer-frame delta between the two readings of the Article 2(7)/Chapter VII kernel.').

omega_variable(
    disagreement_location_protection_trigger,
    'Where exactly do the two readings disagree: on the mechanism (whether force must pass the Council gate) or on the trigger (whether systematic atrocity activates an external responsibility the gate must answer)?',
    'Doctrinal analysis of the 2005 World Summit Outcome Document and subsequent Council practice: both readings accept the Council gate; they divide on whether atrocity constitutes a trigger that the gate''s custodians are obliged to answer.',
    'If the disagreement is trigger-location only, the readings are mechanically compatible and coexistence is strict; if it reaches the mechanism, one reading logically forecloses the other within any single party''s framework and the relation re-types to foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_protection_trigger, conceptual, 'Locates the structural element on which the sibling readings actually diverge.').

omega_variable(
    norm_versus_veto_decomposition,
    'Is the measured extraction a property of the non-intervention norm itself, or of the P5 veto administration layered on top of it?',
    'Counterfactual decomposition: re-model the same mutual-forbearance norm with automatic authorization thresholds (General Assembly supermajority, independent certification panel) and re-measure blocking rates in Rwanda-, Darfur-, and Syria-class cases.',
    'If extraction tracks the veto layer, the constraint decomposes into a low-extraction mutual-forbearance rope plus a high-extraction gatekeeping structure; the fused story''s epsilon is then a weighted sum of two constraints, not an intrinsic value, and the family should be split.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_versus_veto_decomposition, empirical, 'Epsilon-invariance probe: whether the barrier and its veto administration are separable constraints.').

omega_variable(
    anarchic_irreducibility_question,
    'Is the barrier an irreducible feature of anarchic order (no enforcement authority exists above states, so intervention rules cannot bind the strong) or a constructed, revisable legal arrangement?',
    'Comparative institutional analysis of intervention-commitment regimes that held without world government (NATO collective defense, EU mutual assistance clauses) against regimes that collapsed under great-power stress.',
    'If irreducible, the barrier trends toward the fixed end and reform energy systematically misallocates; if constructed, transition-design remedies (standing authorization, veto restraint codes) become structurally available and the persistence question reopens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(anarchic_irreducibility_question, empirical, 'Natural-feature versus constructed-arrangement ambiguity in the barrier''s persistence.').

omega_variable(
    consent_authenticity_laundering,
    'How much of the barrier''s apparent stability rests on coerced or manufactured consent (invited interventions, requests issued by dependent or puppet authorities) rather than genuine domestic assent?',
    'Audit of invitation-based deployments for coercion signatures: force arriving before the request, requests issued by authorities lacking domestic representativeness, requests drafted by the intervening power.',
    'High laundering rates mean the barrier operates as a selective great-power instrument wearing neutral-forbearance clothing, and effective extraction exceeds the authored value; low rates confirm the consent channel as a genuine safety valve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_authenticity_laundering, empirical, 'Whether the consent exception is authentic or systematically laundered.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__sovereignty_first_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t10, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(arti_tr_t10, observed).
narrative_ontology:measurement(arti_tr_t20, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(arti_tr_t20, observed).
narrative_ontology:measurement(arti_tr_t30, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement_basis(arti_tr_t30, observed).
narrative_ontology:measurement(arti_tr_t40, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement_basis(arti_tr_t40, observed).
narrative_ontology:measurement(arti_tr_t50, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 50, 0.36).
narrative_ontology:measurement_basis(arti_tr_t50, observed).
narrative_ontology:measurement(arti_tr_t60, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement_basis(arti_tr_t60, observed).
narrative_ontology:measurement(arti_tr_t70, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 70, 0.44).
narrative_ontology:measurement_basis(arti_tr_t70, observed).
narrative_ontology:measurement(arti_tr_t80, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 80, 0.46).
narrative_ontology:measurement_basis(arti_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t10, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(arti_be_t10, observed).
narrative_ontology:measurement(arti_be_t20, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(arti_be_t20, observed).
narrative_ontology:measurement(arti_be_t30, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement_basis(arti_be_t30, observed).
narrative_ontology:measurement(arti_be_t40, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(arti_be_t40, observed).
narrative_ontology:measurement(arti_be_t50, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(arti_be_t50, observed).
narrative_ontology:measurement(arti_be_t60, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 60, 0.71).
narrative_ontology:measurement_basis(arti_be_t60, observed).
narrative_ontology:measurement(arti_be_t70, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 70, 0.74).
narrative_ontology:measurement_basis(arti_be_t70, observed).
narrative_ontology:measurement(arti_be_t80, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 80, 0.76).
narrative_ontology:measurement_basis(arti_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t10, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement_basis(arti_su_t10, observed).
narrative_ontology:measurement(arti_su_t20, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(arti_su_t20, observed).
narrative_ontology:measurement(arti_su_t30, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(arti_su_t30, observed).
narrative_ontology:measurement(arti_su_t40, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement_basis(arti_su_t40, observed).
narrative_ontology:measurement(arti_su_t50, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 50, 0.66).
narrative_ontology:measurement_basis(arti_su_t50, observed).
narrative_ontology:measurement(arti_su_t60, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 60, 0.74).
narrative_ontology:measurement_basis(arti_su_t60, observed).
narrative_ontology:measurement(arti_su_t70, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 70, 0.7).
narrative_ontology:measurement_basis(arti_su_t70, observed).
narrative_ontology:measurement(arti_su_t80, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 80, 0.64).
narrative_ontology:measurement_basis(arti_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__sovereignty_first_reading, identity_coordination).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension__r2p_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Article 2(7)/Chapter VII tension' fuses two structurally distinct constraints and is decomposed per the epsilon-invariance principle. This story authors the barrier constraint: epsilon fixed to the standing non-intervention arrangement's operation, victims are the shielded populations, beneficiaries are the shielded governments and the gatekeepers. The sibling story (article_2_7_chapter_vii_tension__r2p_reading) authors the protection-duty constraint: epsilon fixed to the protection obligation's non-performance, a different failure mode, a different victim calculus. The upstream-downstream edge runs from this reading to the sibling: every veto-held atrocity case strengthens the barrier reading's grip and starves the R2P reading of operative instances, so this constraint exerts structural pressure on the sibling without foreclosing it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
