% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__composite_reading
 *   human_readable: Honor-Violence Legitimacy Regime — Composite (Drop + Contraction) Reading
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   Across the long nineteenth century (modeled here as interval 0-140,
 *   approximately 1780-1920), the European regime that made violence a
 *   legitimate answer to affronts of honor did not die of one cause. Two
 *   dismantling mechanisms ran at once. The external-cost track: statutes
 *   defining the duel as murder aggravated by conspiracy, courts-martial
 *   clauses, prosecutions of survivors and seconds — each conviction raising
 *   the price of the next meeting. The conceptual-redefinition track: honor
 *   itself was redefined around character, credit, profession, and
 *   respectability, so that the vocabulary in which an affront demanded blood
 *   ceased to be speakable among gentlemen. The composite reading holds both
 *   were necessary: prosecutions failed for two centuries while honor's
 *   content held, and redefinition without enforcement pressure left pockets
 *   of live practice for decades. The two mechanisms also had different
 *   victim sets — the cost track burdened the men who still complied,
 *   stacking legal peril on mortal risk; the redefinition track stranded the
 *   men whose standing was capitalized in the old honor vocabulary. This file
 *   is ONE READING of the kernel honor_violence_legitimacy. The colloquial
 *   question 'why did dueling die?' decomposes, per the epsilon-invariance
 *   principle, into a three-story family: drop_reading (legitimacy intact,
 *   priced out), contraction_reading (redefined into the unthinkable), and
 *   this composite. All three share the same referent — the standing
 *   honor-violence legitimacy arrangement — and author different epsilon
 *   values over it by their own lights; the family is linked through
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   aristocratic_officer_class: primary beneficiary
 *   (institutional/identity_locked) — collects standing rents, polices the
 *   class boundary - court_honor_administrators: agenda setter
 *   (institutional/identity_locked) — adjudicates affronts, certifies
 *   satisfaction - challenged_gentlemen: primary bearer of costs
 *   (moderate/identity_locked) — mortal and legal risk under compulsion -
 *   duel_refusers: bearer of enforcement costs (moderate/constrained) —
 *   ruined for declining - bereaved_families_of_duel_dead: collateral bearers
 *   (moderate/constrained) - state_prosecutors_and_tribunals: adversarial
 *   agenda setter of the cost track (powerful/arbitrage) -
 *   non_gentleman_classes: excluded seat (organized/mobile) — barred from the
 *   honor economy, building rival registers - analytical_historians:
 *   analytical observer (analytical/analytical)
 *
 * KEY AGENTS:
 *   - aristocratic_officer_class: primary beneficiary (institutional/identity_locked) — the class whose boundary and internal hierarchy the code maintained; collects deference and distinction
 *   - court_honor_administrators: agenda_setter (institutional/identity_locked) — seconds, regimental courts of honor, adjudicators of affront and satisfaction
 *   - challenged_gentlemen: payer with secondary beneficiary position (moderate/identity_locked) — bears mortal and legal risk; winning restores and raises standing
 *   - duel_refusers: payer (moderate/constrained) — bears ostracism, dismissal, and the coward's mark for declining
 *   - bereaved_families_of_duel_dead: payer (moderate/constrained) — carry the deaths and ensuing ruin with no standing in the code's adjudication
 *   - state_prosecutors_and_tribunals: agenda_setter of the prohibition regime (powerful/arbitrage) — impose the external costs that constitute the drop mechanism
 *   - non_gentleman_classes: excluded (organized/mobile) — barred from giving or demanding satisfaction; build rival registers of standing
 *   - analytical_historians: observer (analytical/analytical) — reconstruct the arrangement from trial records and correspondence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, 0.52).
domain_priors:suppression_score(honor_violence_legitimacy__composite_reading, 0.34).
domain_priors:theater_ratio(honor_violence_legitimacy__composite_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__composite_reading, "Honor-Violence Legitimacy Regime — Composite (Drop + Contraction) Reading").
narrative_ontology:topic_domain(honor_violence_legitimacy__composite_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__composite_reading, '984e894b-4aaa-4264-aeac-744f56a2b44f').
narrative_ontology:cs_kernel_codification('984e894b-4aaa-4264-aeac-744f56a2b44f', distributed).
narrative_ontology:cs_authority_grounding('984e894b-4aaa-4264-aeac-744f56a2b44f', practice).
narrative_ontology:cs_interpretation_layer_present('984e894b-4aaa-4264-aeac-744f56a2b44f').
narrative_ontology:cs_reading_relation('984e894b-4aaa-4264-aeac-744f56a2b44f', honor_violence_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('984e894b-4aaa-4264-aeac-744f56a2b44f', honor_violence_legitimacy__contraction_reading, influences).
narrative_ontology:cs_axiom('984e894b-4aaa-4264-aeac-744f56a2b44f', foundational, decline_required_both_mechanisms).
narrative_ontology:cs_axiom_status(decline_required_both_mechanisms, holdable).
narrative_ontology:cs_axiom_grounding('984e894b-4aaa-4264-aeac-744f56a2b44f', decline_required_both_mechanisms, empirically_contingent).
narrative_ontology:cs_axiom('984e894b-4aaa-4264-aeac-744f56a2b44f', secondary, victim_sets_structurally_disjoint).
narrative_ontology:cs_axiom_status(victim_sets_structurally_disjoint, holdable).
narrative_ontology:cs_axiom_grounding('984e894b-4aaa-4264-aeac-744f56a2b44f', victim_sets_structurally_disjoint, empirically_contingent).
narrative_ontology:cs_reference_frame('984e894b-4aaa-4264-aeac-744f56a2b44f', regulated_satisfaction_equilibrium).
narrative_ontology:cs_drift_state('984e894b-4aaa-4264-aeac-744f56a2b44f', interwar_europe, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('984e894b-4aaa-4264-aeac-744f56a2b44f', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, aristocratic_officer_class).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, court_honor_administrators).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, challenged_gentlemen).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, duel_refusers).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, bereaved_families_of_duel_dead).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, challenged_gentlemen).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__composite_reading, satisfaction_doctrine).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__composite_reading, code_duello_procedural_authority).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__composite_reading, honour_as_exclusionary_capital).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The European gentry and commissioned ranks as a body. They set the unwritten rules of affront and satisfaction through regimental custom, fashionable opinion, and the example of prominent duellists, and they punished deviation by withdrawing society from offenders. Membership conferred the right to give and demand satisfaction, and the code marked the boundary between gentlemen and everyone else. Individual members could resign commissions or emigrate, but the class as a body had nowhere else to be gentlemen — its standing was constituted by the code it policed.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, aristocratic_officer_class, beneficiary,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__composite_reading, aristocratic_officer_class, agenda_setter).

% Seconds, regimental courts of honor, and the retired officers who adjudicated quarrels. They decided what counted as an insult, what redress sufficed, and whether a meeting had been properly conducted. Their office existed only because the code did; as meetings turned ceremonial, their judgments turned ceremonial with them. Abandoning the code would have abolished their own authority.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, court_honor_administrators, agenda_setter,
    institutional, generational, identity_locked, national).

% Men who received a challenge. Acceptance risked death or maiming and, once prohibited, prosecution; refusal risked social ruin, dismissal from the service, and the permanent mark of cowardice. Victory restored and raised standing, so the same encounter that threatened their lives confirmed their rank. Leaving the honor economy meant leaving the class that defined them.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, challenged_gentlemen, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__composite_reading, challenged_gentlemen, beneficiary).

% Men who declined to fight — on religious grounds, from conscience, or by calculation. They were cut by former friends, passed over for promotion, and forced out of regiments and clubs. Some rebuilt standing through printed explanations, public correspondence, or conspicuous service; the path back was narrow and never guaranteed.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, duel_refusers, payer,
    moderate, biographical, constrained, national).

% Widows, parents, and children of men killed in meetings. They carried the loss and frequently the financial ruin that followed a breadwinner's death. The code gave them no standing to complain, and public mourning for a duellist could shade into scandal; their recourse was petition, pamphlet, and the anti-duelling societies that took up their cases.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, bereaved_families_of_duel_dead, payer,
    moderate, biographical, constrained, regional).

% Royal courts, civilian magistrates, and military tribunals that outlawed the meeting and prosecuted its survivors. They framed duelling as murder aggravated by conspiracy, convicted seconds as well as principals, and wrote anti-duelling clauses into army regulations. Each successful prosecution raised the price of the next meeting; the same offices could rewrite the rules at will, and gained public legitimacy with every conviction.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, state_prosecutors_and_tribunals, agenda_setter,
    powerful, generational, arbitrage, national).

% Tradesmen, laborers, and the urban middle classes, barred from giving or demanding satisfaction by the code's own definitions of who counted as a principal. Radical journalists attacked the practice as aristocrats claiming a private right of war; middle-class men built rival registers of standing — credit, profession, respectability — that required no blood. They were never inside the arrangement, and their organizing power grew across the period.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, non_gentleman_classes, excluded,
    organized, generational, mobile, continental).

% Legal anthropologists and historians of violence who reconstruct the code's operation from trial records, correspondence, and regimental archives. They compare jurisdictions and periods, owe the code no allegiance, and their accounts feed back into how the practice is remembered and taught.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, analytical_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__composite_reading, aristocratic_officer_class).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Regulated lethal quarrelling inside a warrior elite that acknowledged no common superior: fixed procedures for affront, challenge, and satisfaction; capped escalation at a single rule-bound encounter; preserved both parties' standing whatever the outcome; and marked who counted as a man of honor at all.
% TRANSFER_FUNCTION: Moved vindication and standing to those who gave or won satisfaction; moved the mortal and legal costs of quarrel settlement onto individual gentlemen's bodies and liabilities rather than the class collectively; moved adjudication authority to seconds and courts of honor; and, in its final phase, moved the class's distinction claims into print, profession, and credit.
% ABSENT_VOICES: The dead and maimed had no seat in any court of honor; widows and mothers petitioned from outside its jurisdiction; clergy had condemned the practice for centuries and were answered with burial denials and ridicule; the propertyless majority were excluded by definition from giving or demanding satisfaction. All stood outside the code's adjudicating circle because the code defined who qualified as a principal, and none of them qualified.
% DISAPPEARANCE_RATIONALE: When the arrangement went, the world visibly rearranged around its absence: reputation disputes moved into civil courts and the press; officer discipline consolidated in courts-martial; the class's boundary markers migrated to education, profession, and credit registers; affairs that once demanded blood ended in printed apologies or libel actions. The rearrangement is the composite reading's evidence — a world that could absorb the duel's disappearance only because both the price and the meaning had been dismantled together.
% FOUNDING_PROBLEM: Lethal quarrels among armed men who recognised no superior court: an insult unanswered invited contempt, and private vengeance escalated into feud. The code channelled these quarrels into single, rule-bound encounters with honorable exits for both sides.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: legal historians' state-monopoly-of-violence account, military archives showing discipline transferred to courts-martial, and contemporaneous anti-duelling testimony from evangelical societies and prosecuting magistrates that the premises of satisfaction had collapsed. The code's own late adherents attested the opposite — that honor still demanded satisfaction — which is itself evidence that the corroborating sources sit outside the beneficiary set.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_violence_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__composite_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.52 (interval end) with a rise-then-fall arc: 0.58 at the regime's height, peaking at 0.66 around t=60 when the two tracks collided — law forbade what honor compelled, so each remaining participant bore compounded legal-plus-mortal costs — then falling as redefinition emptied the rolls and dissolved the compulsion itself. This arc IS the composite reading's structural delta: the drop mechanism raises per-participant burden while the contraction mechanism shrinks the governed set, and their simultaneous operation produces a squeeze-then-release profile no single-mechanism story generates. Theater ratio rises monotonically to 0.70: late-period meetings were increasingly ceremonial — bloodless political encounters, seconds settling matters without shots, deloping by convention — performance substituting for function as the code's meaning drained away. Suppression requirement follows a ratchet-then-collapse arc (0.55 to 0.71 to 0.34): the series is authored because this story is precisely about enforcement-capacity dynamics — honor culture's counter-pressure ratcheted up against state prohibition, then lost its motive and its means as redefinition removed the reason to enforce. Accessibility collapse is 0.42: alternatives (civil courts, printed apologies, press-mediated vindication) remained usable and grew more respectable, so the arrangement never closed the exit space the way a natural limit would. Resistance is 0.58: refusers, evangelical societies, prosecuting magistrates, and mass politics met the arrangement continuously across the interval. All three series run on one shared eight-point grid; every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the aristocratic_officer_class and court_honor_administrators seats, the code is an inherited constitution: the thing that made them gentlemen and made their quarrels governable. From the challenged_gentlemen and duel_refusers seats, the same rules are compulsion with mortal stakes — men of nominally equal standing experienced radically different constraint depending on which side of a challenge they stood, which is the same-level lateral divergence this story carries. From the state_prosecutors_and_tribunals seat, the practice is administrable crime. From the non_gentleman_classes seat, it is a privileged private war from which they were excluded by definition. The engine derives these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries sit near the subsidized end: the officer class collected deference, boundary maintenance, and internal order; the administrators collected the authority their office was made of. Declared victims sit near the targeted end: challenged gentlemen bore the transfer's mortal and legal costs under identity lock; refusers bore the enforcement mechanism's full weight; bereaved families bore its terminal output. Two directionality overrides are declared because the derivation chain has no structural data for seats outside the beneficiary/victim declaration and would leave them at fallback. The state seat (powerful) is overridden to d=0.40: it is the drop mechanism's author, yet the arrangement imposed real costs on it — enforcement expenditure, jurisdictional conflict with the honor courts — while its campaign yielded legitimacy; net relation is mildly targeted, not symmetric. The non_gentleman_classes seat (organized) is overridden to d=0.55: formally outside the arrangement, yet taxed indirectly by the deference economy it excluded them from; their growing mobility damps the effect. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — regulating lethal quarrels among armed men who acknowledged no superior court — died with the state's monopoly of violence and the redefinition of honor, but the arrangement outlived its mandate as ceremony: the late-interval theater ratio of 0.70 is the signature of function maintained as performance. The classification guards against three mislabelings. Calling the whole arc pure predation ignores the genuine coordination the code performed for two centuries — it capped feuds, preserved face on both sides, and gave a warrior elite a governable quarrel form. Calling it pure coordination ignores the asymmetry: the class collected the standing while individuals paid in blood and prosecutions. Reading its end as a natural death — the arrangement simply expiring like a physical limit — erases the twin engineered mechanisms this reading exists to register. The R5 mismatch signal (founding_problem_status dead combined with disappearance_verdict world_rearranges) flags the ceremonial tail for exactly the zombie-pattern check the theater series corroborates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This constraint is one reading of kernel honor_violence_legitimacy — the composite_reading, which holds that external-cost imposition (drop) and conceptual redefinition of honor (contraction) operated simultaneously. What would change structurally if a sibling reading were instantiated instead?',
    'Instantiate the sibling readings as separate constraint files: drop_reading authors epsilon for a legitimacy-intact arrangement priced out from outside (victim set concentrated on compliers bearing compounded legal and mortal costs); contraction_reading authors epsilon for a legitimacy-dissolved arrangement (victim set concentrated on holders of devalued honor capital). Compare per-seat classifications across the family.',
    'If drop_reading were adopted, the legitimacy structure itself never changed and the arrangement''s beneficiary structure survived intact beneath the price mechanism; if contraction_reading were adopted, external costs were noise around a purely semantic dissolution. The composite''s dual victim sets and its rise-then-fall extractiveness arc would collapse into single-mechanism profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Committer structure: which reading of the honor-violence legitimacy kernel this story instantiates and what siblings would alter.').

omega_variable(
    mechanism_weight_attribution,
    'What share of the observed decline is attributable to each mechanism — external costs versus conceptual redefinition — and were they additive or interactive?',
    'Comparative jurisdictional analysis: correlate the timing of statutory prohibition and prosecution intensity with the timing of honor-vocabulary shifts (etiquette manuals, obituary language, officer-corps admission criteria) across countries where the two tracks moved on different schedules.',
    'A dominant-drop result would push the composite toward the drop_reading''s profile; a dominant-contraction result toward the contraction_reading; an interactive result (each mechanism lowering the other''s threshold) would confirm the composite as a distinct third structure rather than a weighted average.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_weight_attribution, empirical, 'Relative causal weight and interaction structure of the two decline mechanisms.').

omega_variable(
    drop_insufficiency_counterfactual,
    'The composite''s distinguishing claim is that the contraction edge makes the drop mechanism insufficient alone: would dueling have survived indefinitely under external costs alone if honor''s content had stayed fixed?',
    'Examine periods and places where prosecution intensified while the honor code''s content held (early modern royal edicts, seventeenth-century church-court campaigns) against places where honor''s content shifted with lax enforcement. If dueling weathered sustained external costs whenever the code''s meaning held, and faded wherever meaning shifted regardless of enforcement, the insufficiency claim is corroborated.',
    'If some jurisdiction abolished dueling through costs alone with honor''s content intact, the composite weakens to a drop-primary account and the contraction edge is demoted to an accelerant; the claimed foreclosure of the drop_reading''s premise would then be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drop_insufficiency_counterfactual, empirical, 'Whether external costs alone could have killed the practice, or conceptual redefinition was necessary.').

omega_variable(
    victim_set_disjointness,
    'Are the two mechanisms'' victim sets genuinely disjoint — compliers bearing compounded legal-plus-mortal costs under the drop track, versus holders of devalued honor capital under the contraction track — or do they overlap enough to constitute one extraction stream?',
    'Prosopographical analysis of prosecuted duelists and of men recorded as declining satisfaction on redefined-honor grounds in the same decades: do the same individuals and families appear in both rolls, and did the compounded-cost burden fall on the same strata that lost honor capital?',
    'Disjoint sets imply the arrangement ran two distinct extraction regimes simultaneously, sharpening per-seat divergence; heavy overlap would merge the victim structure and simplify the directionality map toward a single targeted population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_disjointness, empirical, 'Whether the drop and contraction mechanisms drew from the same or different victim populations.').

omega_variable(
    mensur_successor_status,
    'Does the German academic fencing culture that outlived the interval count as this arrangement surviving in transformed form (contraction incomplete), or as a successor arrangement with a different kernel?',
    'Compare the Mensur''s normative premises (consensual scar-seeking, no affront-satisfaction linkage, judicially tolerated) against the code duello''s premises (affront-compelled, satisfaction-linked, prosecuted). Continuity of personnel and ritual versus discontinuity of premise decides the case.',
    'If the Mensur is a survivor, the end-state classification carries a live remnant and the contraction mechanism is incomplete at interval end; if it is a successor, the original arrangement terminated cleanly and the end-state metrics describe a closed husk.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mensur_successor_status, conceptual, 'Whether transformed survivals belong to this constraint or to a successor constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__composite_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__composite_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(hono_tr_t0, observed).
narrative_ontology:measurement(hono_tr_t20, honor_violence_legitimacy__composite_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(hono_tr_t20, observed).
narrative_ontology:measurement(hono_tr_t40, honor_violence_legitimacy__composite_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement_basis(hono_tr_t40, observed).
narrative_ontology:measurement(hono_tr_t60, honor_violence_legitimacy__composite_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement_basis(hono_tr_t60, observed).
narrative_ontology:measurement(hono_tr_t80, honor_violence_legitimacy__composite_reading, theater_ratio, 80, 0.5).
narrative_ontology:measurement_basis(hono_tr_t80, observed).
narrative_ontology:measurement(hono_tr_t100, honor_violence_legitimacy__composite_reading, theater_ratio, 100, 0.58).
narrative_ontology:measurement_basis(hono_tr_t100, observed).
narrative_ontology:measurement(hono_tr_t120, honor_violence_legitimacy__composite_reading, theater_ratio, 120, 0.64).
narrative_ontology:measurement_basis(hono_tr_t120, observed).
narrative_ontology:measurement(hono_tr_t140, honor_violence_legitimacy__composite_reading, theater_ratio, 140, 0.7).
narrative_ontology:measurement_basis(hono_tr_t140, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__composite_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(hono_be_t0, observed).
narrative_ontology:measurement(hono_be_t20, honor_violence_legitimacy__composite_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(hono_be_t20, observed).
narrative_ontology:measurement(hono_be_t40, honor_violence_legitimacy__composite_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement_basis(hono_be_t40, observed).
narrative_ontology:measurement(hono_be_t60, honor_violence_legitimacy__composite_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement_basis(hono_be_t60, observed).
narrative_ontology:measurement(hono_be_t80, honor_violence_legitimacy__composite_reading, base_extractiveness, 80, 0.63).
narrative_ontology:measurement_basis(hono_be_t80, observed).
narrative_ontology:measurement(hono_be_t100, honor_violence_legitimacy__composite_reading, base_extractiveness, 100, 0.59).
narrative_ontology:measurement_basis(hono_be_t100, observed).
narrative_ontology:measurement(hono_be_t120, honor_violence_legitimacy__composite_reading, base_extractiveness, 120, 0.55).
narrative_ontology:measurement_basis(hono_be_t120, observed).
narrative_ontology:measurement(hono_be_t140, honor_violence_legitimacy__composite_reading, base_extractiveness, 140, 0.52).
narrative_ontology:measurement_basis(hono_be_t140, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_violence_legitimacy__composite_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(hono_su_t0, observed).
narrative_ontology:measurement(hono_su_t20, honor_violence_legitimacy__composite_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(hono_su_t20, observed).
narrative_ontology:measurement(hono_su_t40, honor_violence_legitimacy__composite_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement_basis(hono_su_t40, observed).
narrative_ontology:measurement(hono_su_t60, honor_violence_legitimacy__composite_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement_basis(hono_su_t60, observed).
narrative_ontology:measurement(hono_su_t80, honor_violence_legitimacy__composite_reading, suppression_requirement, 80, 0.63).
narrative_ontology:measurement_basis(hono_su_t80, observed).
narrative_ontology:measurement(hono_su_t100, honor_violence_legitimacy__composite_reading, suppression_requirement, 100, 0.52).
narrative_ontology:measurement_basis(hono_su_t100, observed).
narrative_ontology:measurement(hono_su_t120, honor_violence_legitimacy__composite_reading, suppression_requirement, 120, 0.42).
narrative_ontology:measurement_basis(hono_su_t120, observed).
narrative_ontology:measurement(hono_su_t140, honor_violence_legitimacy__composite_reading, suppression_requirement, 140, 0.34).
narrative_ontology:measurement_basis(hono_su_t140, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__composite_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__contraction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the decline of dueling' covers three structurally distinct claims and decomposes into three stories sharing the referent (the standing honor-violence legitimacy arrangement) with reading-indexed epsilon values. drop_reading authors epsilon for a legitimacy-intact arrangement priced out from outside — its extraction concentrates on compliers bearing compounded costs, and its profile is monotone burden growth. contraction_reading authors epsilon for a legitimacy-dissolved arrangement — its extraction concentrates on holders of devalued honor capital, and its profile is dissolution. This composite authors epsilon for the dual-track arrangement: two victim sets, a squeeze-then-release extractiveness arc, and the insufficiency claim (contraction edge makes drop alone inadequate) as its distinguishing premise. Upstream/downstream: the drop and contraction stories are typically cited as competing complete explanations; the composite cites both as jointly necessary components. All three files link one to another through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_violence_legitimacy__composite_reading, powerful, 0.4).
constraint_indexing:directionality_override(honor_violence_legitimacy__composite_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
