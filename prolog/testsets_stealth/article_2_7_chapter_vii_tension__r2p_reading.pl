% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__r2p_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__r2p_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: article_2_7_chapter_vii_tension__r2p_reading
 *   human_readable: Responsibility to Protect — Conditional Sovereignty Reading
 *   domain: international law/political philosophy/security studies
 *
 * SUMMARY:
 *   This story instantiates the r2p_reading of the
 *   article_2_7_chapter_vii_tension kernel: sovereignty is conditional on
 *   protecting populations, and systematic atrocity triggers an international
 *   responsibility to respond. The constraint classified here is the R2P norm
 *   AS INSTITUTIONALIZED — the 2005 World Summit form, gated by Security
 *   Council authorization, operating post-Libya — not the contest with its
 *   sibling reading, which is authored as its own constraint and linked via
 *   network.affects_constraints. The manifest's expected delta names 'the
 *   sovereignty norm itself' among the victims; since propositions cannot be
 *   victims, that loss is carried structurally by the
 *   sovereignty_reliant_middle_powers seat, the actors whose security
 *   actually rests on the norm. Claim/metric independence is maintained:
 *   claimed_type tangled_rope states my structural belief (genuine
 *   coordination function closing the Rwanda-gap, asymmetric extraction
 *   through P5 gating, active enforcement); the metrics describe observed
 *   operation. The ε referent is the standing arrangement under contest — R2P
 *   as operated — assessed by this reading's own lights; even the ICISS
 *   tradition judges the operated norm far below its design, hence
 *   high-but-not-maximal ε. The endorsed alternative (a functioning
 *   veto-restraint regime) is NOT the referent. KEY AGENTS (by structural
 *   relationship): - security_council_permanent_five: Agenda setter and
 *   principal collector (institutional/arbitrage) — controls the trigger,
 *   collects legitimation rents - civilian_populations_in_conflict_zones:
 *   Declared beneficiary, contingent recipient, collateral bearer
 *   (powerless/trapped) - targeted_regimes: Primary target of the norm's
 *   activation (organized/trapped) - sovereignty_reliant_middle_powers:
 *   Diffuse shield-erosion payers (moderate/constrained) -
 *   humanitarian_advocacy_organizations: Secondary beneficiaries sustaining
 *   the norm (moderate/constrained) - international_legal_scholars:
 *   Analytical observer — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, 0.66).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__r2p_reading, 0.45).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__r2p_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__r2p_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__r2p_reading, "Responsibility to Protect — Conditional Sovereignty Reading").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__r2p_reading, "international law/political philosophy/security studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__r2p_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__r2p_reading, '9ea3a08f-4209-4f39-bc53-77381e03c5e5').
narrative_ontology:cs_kernel_codification('9ea3a08f-4209-4f39-bc53-77381e03c5e5', fixed_text).
narrative_ontology:cs_authority_grounding('9ea3a08f-4209-4f39-bc53-77381e03c5e5', lineage).
narrative_ontology:cs_interpretation_layer_present('9ea3a08f-4209-4f39-bc53-77381e03c5e5').
narrative_ontology:cs_reading_relation('9ea3a08f-4209-4f39-bc53-77381e03c5e5', article_2_7_chapter_vii_tension__sovereignty_first_reading, coexists_with).
narrative_ontology:cs_axiom('9ea3a08f-4209-4f39-bc53-77381e03c5e5', foundational, sovereignty_conditional_on_population_protection).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_population_protection, holdable).
narrative_ontology:cs_axiom_grounding('9ea3a08f-4209-4f39-bc53-77381e03c5e5', sovereignty_conditional_on_population_protection, deontological).
narrative_ontology:cs_axiom('9ea3a08f-4209-4f39-bc53-77381e03c5e5', secondary, systematic_atrocity_triggers_international_response_duty).
narrative_ontology:cs_axiom_status(systematic_atrocity_triggers_international_response_duty, holdable).
narrative_ontology:cs_axiom_grounding('9ea3a08f-4209-4f39-bc53-77381e03c5e5', systematic_atrocity_triggers_international_response_duty, instrumental).
narrative_ontology:cs_reference_frame('9ea3a08f-4209-4f39-bc53-77381e03c5e5', sovereignty_as_responsibility).
narrative_ontology:cs_drift_state('9ea3a08f-4209-4f39-bc53-77381e03c5e5', post_libya_post_syria_contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9ea3a08f-4209-4f39-bc53-77381e03c5e5', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, security_council_permanent_five).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, humanitarian_advocacy_organizations).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, targeted_regimes).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_reliant_middle_powers).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__r2p_reading, conditional_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__r2p_reading, responsibility_to_protect_principle).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__r2p_reading, human_security_paradigm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five governments holding permanent seats and veto power over Council action. They decide whether the protection responsibility activates in any given crisis: each authorization, sanctions package, or referral passes or dies by their votes. When they or their partners want to act, the norm supplies the legitimating language; when a crisis touches an ally or their own conduct, they withhold agreement. They bear no comparable exposure themselves — the trigger they control cannot easily be turned on them, because they hold the gate.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, security_council_permanent_five, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__r2p_reading, security_council_permanent_five, beneficiary).

% People living under regimes accused of systematic atrocity. They are the population the norm names as its object of concern: when the machinery activates, protection reaches them — safe corridors, no-fly zones, peacekeepers, humanitarian access. When it does not, they remain exposed with no recourse of their own; they cannot convene the Council, cast a vote, or summon a coalition. Where intervention proceeds and then stalls or topples order, they also absorb the aftermath — collapsed services, militia rule, as in Libya after 2011. Leaving means flight into displacement or neighboring states, rarely safety.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, civilian_populations_in_conflict_zones, beneficiary,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__r2p_reading, civilian_populations_in_conflict_zones, payer).

% Governments facing atrocity accusations or intervention campaigns. The norm converts their internal conduct into grounds for external coercion: sanctions listings, arms embargoes, indictment requests, and ultimately authorized force. Once targeted, they cannot exit the category — the norm attaches to territory and population, not to consent — and their choices reduce to defiance, negotiation under pressure, or collapse. Some are genuine perpetrators; others are governments caught in a rival's framing, which they cannot cleanly disprove before the gatekeepers.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, targeted_regimes, payer,
    organized, biographical, trapped, national).

% States without permanent seats or alliance shields whose main security guarantee has always been the rule that borders and internal affairs are inviolable. Every widening of the intervention exception raises their exposure: today's criterion can become tomorrow's pretext applied to them. They answer through coalitions — the Non-Aligned Movement, the G77, the ACT Group — and through counter-proposals such as Brazil's Responsibility while Protecting, but they cannot veto anything and their objections register only as friction. The value they carry for the sovereignty norm itself is concentrated here: they are the actors whose security actually rests on it.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_reliant_middle_powers, payer,
    moderate, generational, constrained, regional).

% NGOs, research centers, and UN-affiliated offices that document atrocities and press for response. The norm gives them a recognized mandate, funding streams, and a seat in policy debate; their annual reporting and campaigning sustain the norm's visibility in return. They influence which crises get framed as protection failures, though they decide nothing and bear none of the consequences of the actions they urge.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, humanitarian_advocacy_organizations, beneficiary,
    moderate, biographical, constrained, global).

% Academic lawyers and political theorists who track the norm's doctrine and practice — the ICISS lineage, the 2005 text, the Libya aftermath, the veto-restraint initiatives. They publish the analyses both camps cite, see the full structure across seats, and hold no stake in any authorization decision.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_2_7_chapter_vii_tension__r2p_reading, security_council_permanent_five).
narrative_ontology:fixing_cost_class(article_2_7_chapter_vii_tension__r2p_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Closes the collective-action gap exposed by Rwanda and Srebrenica: pre-commits an international response threshold for mass atrocity, pools legitimacy so no single state must act alone or improvise legal cover, and routes coercive response through an authorized channel instead of ad hoc unilateralism.
% TRANSFER_FUNCTION: Moves decision rights over the sovereignty shield from individual states to the Security Council — in practice the five veto holders' discretion; moves legitimation-for-force to whichever great power invokes the norm; moves intervention risk onto the targeted territory's population; moves agenda-setting influence to the advocacy organizations that frame crises as protection failures.
% ABSENT_VOICES: The protected populations hold no seat in the authorization process — no vote, no veto, no standing; their protection is decided by parties whose strategic interests rarely track their survival. Civilians inside targeted states speak only through exile networks. G77 legal traditions enter as floor speeches, never as gatekeepers. The 2005 unanimity was eased by selling the trigger as Council-gated — the seats that would bear the coercion were never positioned to refuse it.
% DISAPPEARANCE_RATIONALE: Overnight disappearance restores sovereignty-first as the operative default: intervening powers lose the legitimating vocabulary and would need cruder justifications for force; the advocacy sector loses mandate and funding architecture; persecuted populations lose even the rhetorical hook that has twice (Kenya 2008, Côte d'Ivoire 2011) mobilized response; and the five veto holders lose a discretionary gate they currently monopolize. The humanitarian-intervention discourse, the Council's protective docket, and the norm-entrepreneurship economy would all rearrange.
% FOUNDING_PROBLEM: Post-Cold War atrocity failures — Rwanda 1994, Srebrenica 1995 — exposed that absolute sovereignty left international observers able to describe genocide in real time but structurally unable to stop it. Kofi Annan's 1999 challenge and the ICISS report of 2001 built the responsibility-to-protect framework to close that gap.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's reality is corroborated from outside any benefiting party: ICTR and ICTY judgments and survivor testimony establish the Rwanda and Srebrenica failures; the 2005 consensus included sovereignty-skeptical G77 governments that attested the problem while contesting the remedy; after 2011, Brazil's Responsibility while Protecting initiative and the cross-regional ACT Group attested — from outside the Western beneficiary set — that the current arrangement falls short of its own standard. No party outside the beneficiary set attests that the present Council-gated arrangement solves the problem; that absence is itself signal.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__r2p_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__r2p_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__r2p_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__r2p_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__r2p_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.66) because the norm's operative value — when sovereignty's shield holds and when it fails — is appropriated by the veto-holding gatekeepers and lent selectively as legitimation for force, while its costs land on targeted regimes, shield-dependent middle powers, and the very populations protection is meant to serve. Suppression (0.45) is moderate: the coercive apparatus (authorized force, sanctions, referrals) now fires rarely, but the discursive layer persists — opposing intervention is framed as siding with perpetrators, and unilateral alternatives sit largely foreclosed. Theater (0.52) crosses the Goodhart line: annual Secretary-General reports, summit language, and commemorative infrastructure outweigh delivered protection in volume. Accessibility collapse (0.40): alternatives persist — consent-based access, regional action such as ECOWAS in Gambia 2017, General Assembly 'Uniting for Peace' — but each carries higher legitimacy cost than the Council route. Resistance (0.65) is organized and explicit: Russian and Chinese vetoes, Brazil's RwP, the ACT Group's code-of-conduct campaign, standing G77 objections. Suppression is authored as a raw structural property; only extractiveness is scaled by the engine (by directionality and scope). The three measurement series share one time grid (t=0..24 in four-year steps from the 2001 ICISS report, so t=12 is 2013 and t=24 is 2025). suppression_requirement is included because the story tracks enforcement-capacity change: a ratchet into the 2011 Libya authorization, then decay as P5 consensus collapsed — not a static picture. Fixing cost is prohibitive: every proposed repair (veto restraint, codes of conduct) has stalled for over a decade because the gatekeepers would have to bind themselves, and the cost of that exceeds anything the fixers individually bear.
 *
 * PERSPECTIVAL GAP:
 *   From the P5 seat the norm is a discretionary instrument they built and gate — coordination they administer. From the targeted-regime seat it is a sword suspended over regime survival. From the middle-power seat it is precedent erosion of the only shield formal sovereign equality ever gave them — note these are same-level actors under nominally equal sovereignty whose exit options diverge entirely on possession of the veto (arbitrage versus constrained). From the civilian seat it is a lottery: rescue when great-power interests align (Benghazi 2011), abandonment when they do not (Syria from 2013), harm when intervention destabilizes (post-Gaddafi Libya). Coalition note: middle-power resistance aggregates through the G77 and the ACT Group, giving states that are individually voiceless inside the Council organized voice outside it — the relevant coalition-power consideration for the weaker seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The P5 derive near-beneficiary directionality: declared beneficiaries with arbitrage-grade exit who additionally control the trigger — the constraint cannot extract from the hand that operates it. Advocacy organizations sit low-d: funded and mandated by the norm, bearing none of its consequences. Civilian populations are the deliberate override case: the beneficiary declaration alone would derive strong subsidy, but trapped exit, zero trigger agency, and collateral exposure from the interventions conducted in their name place them near symmetric — hence the powerless-atom override to 0.45. Targeted regimes derive near-full-target d: trapped, shield removed, no appeal above the gatekeepers. Middle powers derive high d: they pay continuously in shield erosion, with only constrained coalition exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem remains live — atrocity risk persists in Syria, Ethiopia, Myanmar, and Sudan — so the mandate has not outlived its function and mandatrophy is not resolved; the arrangement persists while delivering well below design intent. Classification discipline cuts both ways: a snare verdict would erase the genuine coordination wins (Kenya 2008 mediation, Côte d'Ivoire 2011, initial Benghazi protection, Gambia 2017); a rope verdict would erase the P5 capture and the shield erosion borne by every non-veto-holding state. Tangled rope holds both facts simultaneously. The theater_ratio crossing 0.5 flags Goodhart drift — norm maintenance displacing protection delivery — without collapsing the type toward piton, because functional delivery, though a minority share, is nonzero and the enforcement machinery retains latent capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the article_2_7_chapter_vii_tension kernel; the sibling sovereignty_first_reading assigns opposite polarity — under it the sovereignty shield is the protected good and persecuted populations are the abandoned. Which reading''s beneficiary/victim structure should drive classification?',
    'Comparative classification across both reading files: locate the disagreement in the conditionality premise (whether sovereignty''s shield is defeasible on atrocity grounds) and check which structure the operative record — veto patterns, authorization history — actually rewards.',
    'If the sovereignty-first reading prevailed institutionally, this constraint''s beneficiaries and victims invert, the ε referent shifts to the abandonment arrangement, and the computed type recomputes from the inverted structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a two-reading kernel; classification is reading-indexed.').

omega_variable(
    p5_gate_capture_or_design,
    'Is the norm''s selectivity a capture pathology layered onto a sound design (correctable by veto restraint), or inherent to any Council-gated trigger (the 2005 compromise purchased universality by selling the trigger)?',
    'Track uptake and effect of veto-restraint instruments — the France–Mexico political declaration, the ACT Group code of conduct — including whether signatories restrain themselves when an ally is the prospective target.',
    'If capture, reform could pull effective extraction down toward coordination-cost range and soften the tangled-rope asymmetry; if inherent, high extraction persists under any realistic institutionalization and the norm trends toward its extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(p5_gate_capture_or_design, empirical, 'Whether selectivity is correctable pathology or a structural property of the P5 gate.').

omega_variable(
    intervention_net_efficacy,
    'Does R2P-invoked intervention protect populations net of aftermath — Kosovo''s eventual stabilization versus Libya''s collapse and reopened slave markets?',
    'Structured comparison of invoked cases (Kenya 2008, Côte d''Ivoire 2011, Libya 2011, Gambia 2017) on civilian-protection outcomes at one, five, and ten years, against matched non-invoked counterfactuals.',
    'If net-harm cases dominate, the civilian seat flips toward victim and the norm trends snare; if protective, the coordination function is confirmed and extraction sits lower than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_net_efficacy, empirical, 'The beneficiary status of protected populations is contingent on contested efficacy evidence.').

omega_variable(
    maximal_reading_foreclosure,
    'At maximal-strength formulations the two readings'' foundational axioms contradict outright (unconditional sovereignty versus atrocity-defeasible sovereignty) — no single framework could hold both; as institutionally adopted (Council-gated conditionality inside the 2005 text) they coexist. Which relation governs?',
    'Doctrinal analysis of whether any authoritative framework — the 2005 Outcome Document, Court jurisprudence, state practice — holds both premises without interpretively dissolving one of them.',
    'If strict foreclosure holds, the declared coexists_with edge flips and cross-reading comparison changes shape; if coexistence holds, the two-file decomposition stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maximal_reading_foreclosure, conceptual, 'Foreclosure ambiguity between the maximal and institutionalized forms of the two readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__r2p_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t4, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement_basis(arti_tr_t4, observed).
narrative_ontology:measurement(arti_tr_t8, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(arti_tr_t8, observed).
narrative_ontology:measurement(arti_tr_t12, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement_basis(arti_tr_t12, observed).
narrative_ontology:measurement(arti_tr_t16, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 16, 0.48).
narrative_ontology:measurement_basis(arti_tr_t16, observed).
narrative_ontology:measurement(arti_tr_t20, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement_basis(arti_tr_t20, observed).
narrative_ontology:measurement(arti_tr_t24, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 24, 0.52).
narrative_ontology:measurement_basis(arti_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t4, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 4, 0.36).
narrative_ontology:measurement_basis(arti_be_t4, observed).
narrative_ontology:measurement(arti_be_t8, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement_basis(arti_be_t8, observed).
narrative_ontology:measurement(arti_be_t12, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement_basis(arti_be_t12, observed).
narrative_ontology:measurement(arti_be_t16, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement_basis(arti_be_t16, observed).
narrative_ontology:measurement(arti_be_t20, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(arti_be_t20, observed).
narrative_ontology:measurement(arti_be_t24, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(arti_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t4, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement_basis(arti_su_t4, observed).
narrative_ontology:measurement(arti_su_t8, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement_basis(arti_su_t8, observed).
narrative_ontology:measurement(arti_su_t12, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement_basis(arti_su_t12, observed).
narrative_ontology:measurement(arti_su_t16, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement_basis(arti_su_t16, observed).
narrative_ontology:measurement(arti_su_t20, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(arti_su_t20, observed).
narrative_ontology:measurement(arti_su_t24, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 24, 0.45).
narrative_ontology:measurement_basis(arti_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__r2p_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension__sovereignty_first_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Article 2(7) versus Chapter VII tension' covers two structurally distinct constraints. This file instantiates the r2p_reading: sovereignty defeasible on atrocity grounds, ε authored high for the Council-gated arrangement as operated. The sibling file instantiates the sovereignty_first_reading: the shield near-absolute, its ε authored over the abandonment arrangement its own lights expose. Shared kernel: the Charter text. Neither reading is strictly upstream — the historically prior operative default (sovereignty-first) shaped the conditions under which R2P was negotiated, and R2P's adoption raised the legitimacy price of pure sovereignty defenses — so the family is recorded as mutual linkage rather than a single directional edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_2_7_chapter_vii_tension__r2p_reading, powerless, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
