% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__humanitarian_ceiling_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__humanitarian_ceiling_reading, []).

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
 *   constraint_id: geneva_conventions_1949__humanitarian_ceiling_reading
 *   human_readable: Geneva Conventions 1949 — Humanitarian Ceiling Reading (Absolute Minimums Without Reciprocity)
 *   domain: international_law/law_of_armed_conflict/political_philosophy
 *
 * SUMMARY:
 *   A contested kernel — the Geneva Conventions of 1949 with their Additional
 *   Protocols — carries three rival readings of what the texts are. This file
 *   instantiates exactly one: the humanitarian_ceiling_reading, under which
 *   the conventions establish absolute minimum treatment binding every party
 *   regardless of adversary conduct, with no reciprocity condition and no
 *   operational-necessity override. The epsilon authored here refers to the
 *   standing arrangement under contest — state practice under the proclaimed
 *   unconditional floor — as the ceiling reading itself assesses it: a
 *   bounded but real transfer of tactical freedom from state militaries to
 *   protected classes, eroded at the margins by necessity carve-outs,
 *   interpretive drift, and selective enforcement, with the reading counting
 *   the promise-versus-delivery gap as the arrangement's characteristic
 *   failure. The sibling readings are separate constraints with separately
 *   authored epsilon values, not views folded into this one; the family is
 *   linked through the network section. KEY AGENTS (by structural
 *   relationship): - state_militaries: Primary target (institutional/trapped)
 *   — bears the asymmetric operational and legal burden -
 *   individual_service_members: Dual-positioned bearer (moderate/constrained)
 *   — owes and draws the same protections - protected_persons_civilians:
 *   Primary beneficiary (powerless/trapped) - detainees_and_wounded:
 *   Beneficiary (powerless/trapped) - irregular_armed_groups: Concentrated
 *   incidental recipient (organized/mobile) — retains the floor without
 *   symmetric duties - icrc: Administrator-guardian
 *   (institutional/constrained) - war_crimes_tribunals: Enforcer
 *   (institutional/constrained) - operational_necessity_advocates: Excluded
 *   voice (powerful/arbitrage) - ihl_monitoring_bodies: Analytical observer
 *   (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, 0.52).
domain_priors:suppression_score(geneva_conventions_1949__humanitarian_ceiling_reading, 0.62).
domain_priors:theater_ratio(geneva_conventions_1949__humanitarian_ceiling_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__humanitarian_ceiling_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__humanitarian_ceiling_reading, "Geneva Conventions 1949 — Humanitarian Ceiling Reading (Absolute Minimums Without Reciprocity)").
narrative_ontology:topic_domain(geneva_conventions_1949__humanitarian_ceiling_reading, "international_law/law_of_armed_conflict/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__humanitarian_ceiling_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__humanitarian_ceiling_reading, 'e24d8d0e-8330-4fc6-b626-4866896c16cb').
narrative_ontology:cs_kernel_codification('e24d8d0e-8330-4fc6-b626-4866896c16cb', fixed_text).
narrative_ontology:cs_authority_grounding('e24d8d0e-8330-4fc6-b626-4866896c16cb', lineage).
narrative_ontology:cs_interpretation_layer_present('e24d8d0e-8330-4fc6-b626-4866896c16cb').
narrative_ontology:cs_reading_relation('e24d8d0e-8330-4fc6-b626-4866896c16cb', geneva_conventions_1949__conditional_reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('e24d8d0e-8330-4fc6-b626-4866896c16cb', geneva_conventions_1949__security_maximization_reading, forecloses).
narrative_ontology:cs_axiom('e24d8d0e-8330-4fc6-b626-4866896c16cb', foundational, hors_de_combat_protection_is_unconditional).
narrative_ontology:cs_axiom_status(hors_de_combat_protection_is_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('e24d8d0e-8330-4fc6-b626-4866896c16cb', hors_de_combat_protection_is_unconditional, deontological).
narrative_ontology:cs_axiom('e24d8d0e-8330-4fc6-b626-4866896c16cb', foundational, civilian_immunity_preempts_military_necessity).
narrative_ontology:cs_axiom_status(civilian_immunity_preempts_military_necessity, holdable).
narrative_ontology:cs_axiom_grounding('e24d8d0e-8330-4fc6-b626-4866896c16cb', civilian_immunity_preempts_military_necessity, deontological).
narrative_ontology:cs_axiom('e24d8d0e-8330-4fc6-b626-4866896c16cb', secondary, irregular_fighters_keep_common_article_3_floor).
narrative_ontology:cs_axiom_status(irregular_fighters_keep_common_article_3_floor, holdable).
narrative_ontology:cs_axiom_grounding('e24d8d0e-8330-4fc6-b626-4866896c16cb', irregular_fighters_keep_common_article_3_floor, conventional).
narrative_ontology:cs_reference_frame('e24d8d0e-8330-4fc6-b626-4866896c16cb', absolute_floor_without_necessity_qualifier).
narrative_ontology:cs_drift_state('e24d8d0e-8330-4fc6-b626-4866896c16cb', contemporary_asymmetric_war_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e24d8d0e-8330-4fc6-b626-4866896c16cb', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, protected_persons_civilians).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, detainees_and_wounded).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_armed_groups).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, individual_service_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, individual_service_members).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, icrc).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_armed_groups).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__humanitarian_ceiling_reading, absolute_humanitarian_floor_doctrine).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__humanitarian_ceiling_reading, no_reciprocity_principle).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__humanitarian_ceiling_reading, common_article_3_non_international_application).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The armed services of states that ratified the conventions. They translate the texts into military law, rules of engagement, and training curricula; they investigate and court-martial their own personnel for grave breaches. When adversaries disregard the rules entirely, they still carry the duties — closing with the enemy by costlier means, screening fires under precaution requirements, running large detention operations under inspection. Renouncing the regime would strip away the protections their own members depend on when captured; no government has found a survivable way out.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries, payer,
    institutional, generational, trapped, global).

% Soldiers, sailors, and aviators who carry the duties personally: liable to prosecution for violations, ordered to accept added risk rather than shift it onto civilians, evaluated on conduct compliance for advancement. The same body of law fixes how an enemy must treat them if captured — the protection they owe is the protection they draw.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, individual_service_members, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__humanitarian_ceiling_reading, individual_service_members, beneficiary).

% People living where the fighting happens. The texts bar direct attacks on them, demand precautions from attackers, guarantee relief convoys, and license neutral organizations to reach them. They consented to none of it and can enforce none of it; what arrives is what belligerents concede and outside verification manages to catch.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, protected_persons_civilians, beneficiary,
    powerless, biographical, trapped, global).

% Prisoners of war, civilian internees, the sick, and the shipwrecked — people in the enemy's hands. The conventions specify food, shelter, labor limits, correspondence rights, camp inspection, and release at war's end. They hold no bargaining position; their treatment is the detaining power's concession plus whatever inspections detect.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, detainees_and_wounded, beneficiary,
    powerless, biographical, trapped, global).

% Insurgent and militia forces in civil and cross-border wars. Common Article 3 extends them minimum guarantees without requiring prisoner-of-war status in return, and their state opponents stay fully bound while they are not symmetrically so. They profit on the battlefield when the other side fights with restricted means, and they answer under domestic and international law for their own atrocities. Organization runs from disciplined commands to loose fragments; membership turns over quickly.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_armed_groups, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_armed_groups, payer).

% The neutral Swiss-rooted custodian of the texts: it visits detention camps, relays prisoner messages, proposes revisions to the texts, and presses for compliance privately with governments and publicly when private argument fails. Its access exists at belligerent invitation; its budget and standing scale with the regime's reach.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, icrc, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__humanitarian_ceiling_reading, icrc, beneficiary).

% International and hybrid courts prosecuting grave breaches — the Yugoslavia and Rwanda tribunals, the International Criminal Court. Their statutes incorporate the conventions wholesale. They try whom states surrender or refer, which tilts their dockets toward the defeated, the consenting, and the geographically available.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, war_crimes_tribunals, agenda_setter,
    institutional, generational, constrained, continental).

% Defense officials, military lawyers, and doctrine writers who hold that protections must bend to mission requirements against enemies who ignore every rule — wider strike authorities, interrogation programs, detention short of the full prisoner-of-war package. The 1949 texts stripped necessity qualifiers from the core protections, so their position lives at the interpretive margins: memoranda, reservations, coalition diplomacy, doctrinal language. Under the ceiling reading their premise never reaches the table.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, operational_necessity_advocates, excluded,
    powerful, biographical, arbitrage, national).

% UN commissions of inquiry, special rapporteurs, and treaty-body monitors that document conduct in ongoing wars, publish findings, and route evidence toward prosecution. They decide nothing and enforce nothing directly; their product is the record that other seats act on or ignore.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, ihl_monitoring_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_armed_groups).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__humanitarian_ceiling_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes shared minimum-treatment standards for persons hors de combat across all parties to a conflict: identification, humane detention, wounded collection, medical access, and message relay — solving a trust problem no belligerent can solve alone, because treatment standards bind only if each side expects the other to honor them even in defeat.
% TRANSFER_FUNCTION: Moves tactical freedom and risk-absorption obligations from state militaries and their political leadership to protected classes — civilians, detainees, the wounded — and, where adversaries reject the rules, converts state self-restraint into relative battlefield advantage for unbound irregular forces.
% ABSENT_VOICES: Operational-necessity advocates sit inside the paying institutions but outside this reading's conversation: the texts were written to leave their premise no seat, so their objections surface as interpretive pressure rather than counted votes. Beside them stand the unrepresented: victims of violations whose perpetrators were never prosecuted, and the dead, whose interests enter only through the record monitors keep. Where they are: in defense ministries drafting necessity doctrine, in capitals resisting court referral, and nowhere on the enforcement dockets.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would unwind the machinery that runs on the texts: no mandated camp inspection or prisoner-message relay, no agreed detention standards to train against, no grave-breach definitions for courts to prosecute, no neutral channel for negotiations. Captor discretion would expand everywhere at once, wounded-collection and relief systems would lose their legal footing, and the reprisal spirals the 1949 drafters had just watched would resume wherever wars burn longest. The world would rearrange around the absence within a single conflict cycle.
% FOUNDING_PROBLEM: After 1939-45, reciprocity-based laws of war had failed catastrophically: industrial slaughter of civilians, systematic mistreatment of prisoners, and civil-war atrocities lay beyond any bargain, because the worst perpetrators had already defected. The 1949 conferences rebuilt the law around a floor designed to hold even when reciprocity collapsed — protecting persons in enemy hands and civilians in war zones unconditionally.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists outside the beneficiary set but no witness is untouched by the regime. The strongest external attestations come from the paying parties themselves: national defense colleges and judge-advocate curricula teach the atrocity record as the reason the rules exist, and militaries that bear the burden still train to it — attestation against interest from the burden-bearing seat. The Nuremberg and Tokyo trial records independently establish the pre-1949 vacuum, and ICRC detention-visit statistics document continuing need. The honest caveat: every institution positioned to attest the founding problem exists because of it, so corroboration is broad but not disinterested.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__humanitarian_ceiling_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__humanitarian_ceiling_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_1949__humanitarian_ceiling_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.52: bounded — the texts restrict means and methods, not war itself — but real and asymmetrically placed, since the burden concentrates on the bound party in asymmetric conflicts. Suppression is 0.62 and is authored as a raw structural property, unscaled: the 1949 drafters stripped necessity qualifiers from core protections, foreclosing the standard escape clause of earlier war law, and enforcement runs through incorporated domestic military law, courts-martial, and now international prosecution. Theater is 0.40 and climbing: compliance reporting, periodic reviews, and symbolic investigations have grown faster than delivered protection, a textbook proxy-drift signature — the annual report substitutes for the camp visit. Accessibility_collapse is 0.55: for a signatory military, unbound warfare is professionally and legally unavailable, but interpretive exits (reservation politics, definitional narrowing, covert practice) remain open at the margin, so collapse is substantial yet short of natural-law totality. Resistance is 0.60: necessity doctrine persists inside paying institutions, adversaries exploit the asymmetry deliberately, and strong states resist jurisdiction. The temporal series shares one eight-point grid (1949-2026) so every metric is authored at every examined time point. Extraction climbed as the regime matured and asymmetric conflicts concentrated the burden; suppression rose steeply through the enforcement-institutionalization decades (grave-breaches regime, tribunals, domestic incorporation) then eased slightly as major-power immunity and enforcement fatigue set in; theater rose monotonically throughout. The mild post-2005 non-monotonicity reflects drawdown and selectivity, not an oscillation-driven extraction mechanism — no intermittent-reinforcement dynamic is asserted. The 2026 row is authored as a current-year completion estimate and carries projected basis flags.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the state-military seat the arrangement is costly duty shaded by unfairness: casualties accepted for adversaries who cheat, prosecutors waiting at home. From the civilian and detainee seats it is a thin but real shield whose value depends entirely on enemy virtue — protection received is indistinguishable, from inside, from mercy granted. From the irregular-groups seat it is windfall plus exposure: the floor shields them while their opponents' hands stay tied. From the excluded necessity-advocate seat the whole structure looks like doctrinal blindness — a refusal to price protection against mission survival. And from the guardian and enforcer seats it is a mandate whose authority depends on the floor remaining unconditional. Identity-lock matters at the payer seat: professional militaries have fused the lawful-warrior ethos into promotion, education, and organizational self-conception, so exit is not merely legally barred but professionally unthinkable — the institution has become its compliance function. If that identity frame broke, the military seat would move from trapped toward constrained and its computed burden would fall accordingly. The engine derives all of this from power, horizon, exit, and scope; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to low directionality: protected civilians and detainees are subsidized seats — the arrangement delivers to them and takes nothing they hold. Irregular armed groups are declared beneficiaries and additionally mobile, damping their directionality further despite formal Common Article 3 liability; their secondary payer role registers the prosecution exposure that keeps them from the pure beneficiary pole. Declared victims map toward the target end: state militaries are institutional but trapped — denunciation is not a survivable option — and trapped targets sit near full-target directionality, which is exactly what the ceiling reading's unconditional-obligation premise predicts: compliance owed regardless of return. Individual service members inherit the same pull with partial offset from their own prisoner protections. The ICRC derives a mid-to-low value as administering guardian; tribunals similar. No directionality overrides were needed: the beneficiary/victim declarations plus exit options already produce the structure, and the reading's defining feature — unconditionality — is what pins the payer seat at the target end despite partial returns such as legitimacy dividends and own-troop coverage.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a reciprocity-proof floor after industrial atrocity — remains live: armed conflict persists in forms the drafters never imagined, and every paying military still trains to the rules, so the R5 status-times-disappearance read (live x world_rearranges) shows no zombie mismatch. Mandatrophy discipline separates two errors this arrangement invites. First, reading the rising theater ratio as proof the floor is dead and the regime purely vestigial: the coordination core still delivers detention standards, inspection access, and relief footing that visibly rearrange the world when withdrawn, so a piton verdict would be premature. Second, reading the enforcement selectivity as mere imperfection in an otherwise pure coordination device: the asymmetry is structural, located in the same architecture that delivers the protection — bound states financing a floor their unbound adversaries exploit. The tangled-rope claim holds both facts in one structure: genuine coordination with asymmetric extraction, held up by active enforcement, degrading at the edges into performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint instantiates the humanitarian_ceiling_reading of kernel geneva_conventions_1949. Which of the three rival readings governs a given state''s actual obligations, and what changes structurally under each sibling?',
    'Adjudication by treaty interpretation: compare ICRC commentary consensus, tribunal holdings, and observable state doctrine on whether protections are conditioned on adversary conduct or necessity. A state whose doctrine conditions applicability on reciprocity is operating the conditional_reciprocity_reading, not this one.',
    'Under the conditional_reciprocity_reading the victim set widens to include everyone an unbound adversary harms and the payer seat''s directionality falls with adversary compliance; under the security_maximization_reading the suppression of necessity rationales collapses and the payer burden approaches zero while the unprotected swell the victim rolls. The tangled_rope classification authored here holds only under the ceiling reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: one kernel, three readings; this file authors the ceiling reading alone and routes the disagreement to this omega.').

omega_variable(
    enforcement_selectivity_asymmetry,
    'Does the regime''s enforcement machinery bind strong states equally, or does selective prosecution convert the proclaimed floor into a burden that lands hardest on defeated, weak, and consenting jurisdictions?',
    'Comparative docket analysis of war-crimes prosecutions by belligerent power; ICC jurisdiction-referral patterns; universal-jurisdiction case outcomes stratified by defendant state strength.',
    'If enforcement is systematically selective, the effective burden concentrates on weak belligerents while strong-state payers face nominal-only exposure — pushing the weak-party seat''s computed classification toward the extractive end and undermining the universality premise of the ceiling reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_asymmetry, empirical, 'Whether victor''s justice and jurisdiction gaps hollow the floor''s equal application.').

omega_variable(
    compliance_causality_vs_theater,
    'How much of observed restraint in state conduct is caused by the conventions'' operation, versus produced by self-interest, capability limits, and domestic opinion that would restrain belligerents anyway?',
    'Natural experiments comparing conduct across conflicts matched on stakes but differing in regime exposure; within-state conduct variation around accession dates; structured counterfactual analysis of sieges and detention practices.',
    'Low causal contribution inflates the effective weight of the theater ratio and pushes the arrangement toward inertial persistence; high causal contribution secures the coordination-function leg that the tangled_rope reading depends on.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compliance_causality_vs_theater, empirical, 'Separating the floor''s working share from restraint that would occur regardless.').

omega_variable(
    asymmetric_burden_status,
    'Is the state military''s asymmetric burden — honoring rules its adversary rejects — an unjust levy on the paying seat, or the constitutive price of the civilian-protection good?',
    'Not resolvable by data alone; resolved by prior normative commitment to the ceiling reading''s deontological axiom versus consequentialist accounting of tactical cost. Track which framing dominates in doctrine reviews and treaty negotiation positions.',
    'Treating the burden as a levy strengthens the extraction reading of the payer seat; treating it as legitimate design cost supports a purer coordination reading in which the asymmetry is the point rather than a defect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_burden_status, preference, 'Preference-level ambiguity in whether the asymmetry is defect or design.').

omega_variable(
    suppression_internalization_mix,
    'How much of the suppression of necessity rationales is structural (courts-martial, command accountability, incorporated treaty law) versus internalized (professional military ethics fusing the lawful-warrior identity with compliance)?',
    'Compare conduct across militaries with identical treaty exposure but different depths of professional-ethos institutionalization; study conduct drift in forces undergoing rapid demilitarization or doctrinal rupture.',
    'If suppression is heavily internalized, it persists even as enforcement machinery decays and the arrangement resists inertial degradation far longer than its enforcement record predicts; if structural, enforcement decay releases necessity reasoning quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mix, empirical, 'Structural versus internalized composition of the necessity-rationale lockout.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__humanitarian_ceiling_reading, 1949, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1949, 0.12).
narrative_ontology:measurement_basis(gene_tr_t1949, observed).
narrative_ontology:measurement(gene_tr_t1961, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1961, 0.15).
narrative_ontology:measurement_basis(gene_tr_t1961, observed).
narrative_ontology:measurement(gene_tr_t1977, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1977, 0.19).
narrative_ontology:measurement_basis(gene_tr_t1977, observed).
narrative_ontology:measurement(gene_tr_t1989, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1989, 0.22).
narrative_ontology:measurement_basis(gene_tr_t1989, observed).
narrative_ontology:measurement(gene_tr_t1998, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1998, 0.26).
narrative_ontology:measurement_basis(gene_tr_t1998, observed).
narrative_ontology:measurement(gene_tr_t2005, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2005, 0.34).
narrative_ontology:measurement_basis(gene_tr_t2005, observed).
narrative_ontology:measurement(gene_tr_t2014, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2014, 0.38).
narrative_ontology:measurement_basis(gene_tr_t2014, observed).
narrative_ontology:measurement(gene_tr_t2026, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2026, 0.4).
narrative_ontology:measurement_basis(gene_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1949, 0.3).
narrative_ontology:measurement_basis(gene_be_t1949, observed).
narrative_ontology:measurement(gene_be_t1961, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1961, 0.37).
narrative_ontology:measurement_basis(gene_be_t1961, observed).
narrative_ontology:measurement(gene_be_t1977, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1977, 0.44).
narrative_ontology:measurement_basis(gene_be_t1977, observed).
narrative_ontology:measurement(gene_be_t1989, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1989, 0.47).
narrative_ontology:measurement_basis(gene_be_t1989, observed).
narrative_ontology:measurement(gene_be_t1998, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1998, 0.5).
narrative_ontology:measurement_basis(gene_be_t1998, observed).
narrative_ontology:measurement(gene_be_t2005, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement_basis(gene_be_t2005, observed).
narrative_ontology:measurement(gene_be_t2014, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2014, 0.53).
narrative_ontology:measurement_basis(gene_be_t2014, observed).
narrative_ontology:measurement(gene_be_t2026, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2026, 0.52).
narrative_ontology:measurement_basis(gene_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1949, 0.22).
narrative_ontology:measurement_basis(gene_su_t1949, observed).
narrative_ontology:measurement(gene_su_t1961, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1961, 0.29).
narrative_ontology:measurement_basis(gene_su_t1961, observed).
narrative_ontology:measurement(gene_su_t1977, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1977, 0.4).
narrative_ontology:measurement_basis(gene_su_t1977, observed).
narrative_ontology:measurement(gene_su_t1989, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1989, 0.45).
narrative_ontology:measurement_basis(gene_su_t1989, observed).
narrative_ontology:measurement(gene_su_t1998, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1998, 0.57).
narrative_ontology:measurement_basis(gene_su_t1998, observed).
narrative_ontology:measurement(gene_su_t2005, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2005, 0.66).
narrative_ontology:measurement_basis(gene_su_t2005, observed).
narrative_ontology:measurement(gene_su_t2014, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2014, 0.64).
narrative_ontology:measurement_basis(gene_su_t2014, observed).
narrative_ontology:measurement(gene_su_t2026, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2026, 0.62).
narrative_ontology:measurement_basis(gene_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__humanitarian_ceiling_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__security_maximization_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Geneva Conventions' conflates three structurally distinct claims about what the 1949 kernel is; per epsilon-invariance they are three files, not one story with a parameter. This file authors the humanitarian_ceiling_reading: an unconditional floor whose epsilon is indexed to the standing arrangement (state practice under the proclaimed absolute minimums) as the ceiling reading itself assesses it. The conditional_reciprocity_reading authors a different constraint over the same text — victim set expanded by adversary defection, payer burden tracking adversary compliance; the security_maximization_reading authors a third — payer burden near zero, unprotected civilian classes swelling the victim roll. The ceiling reading is upstream: it supplies the doctrinal baseline the reciprocity reading modifies and the security reading suspends. Each file is epsilon-stable within its own referent; divergence between their epsilon values is the family's signal, not an inconsistency to reconcile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
