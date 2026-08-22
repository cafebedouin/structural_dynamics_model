% ============================================================================
% CONSTRAINT STORY: nicene_christological_kernel__homoiousios_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_christological_kernel__homoiousios_reading, []).

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
 *   constraint_id: nicene_christological_kernel__homoiousios_reading
 *   human_readable: Homoiousios Settlement of the Nicene Christological Kernel
 *   domain: historical_theology/ecclesiastical_authority
 *
 * SUMMARY:
 *   A mid-fourth-century ecclesiastical settlement orders the eastern
 *   churches around a christological formula declaring the Son homoiousios —
 *   of similar substance — with the Father: truly divine, truly begotten, yet
 *   ontologically distinct from the Father's underived essence, so that
 *   monotheistic clarity (one underived source) is preserved without
 *   collapsing Father and Son into one person. The arrangement is maintained
 *   by standing synods of eastern bishops drafting subscribable formulas, by
 *   metropolitan networks installing compliant clergy, and — decisively after
 *   359 — by imperial convocation and enforcement (deposition, exile,
 *   troop-backed installation). Its coordination function is real: it gives
 *   the large majority of eastern bishops a formula they can affirm in good
 *   conscience and makes common worship and conciliar action possible across
 *   jurisdictions. Its extraction is also real: dissenters at both ends
 *   (confessors of the same-substance reading; radicals denying the Son's
 *   substance-kinship altogether) pay with office, livelihood, and sometimes
 *   life, and provincial congregations absorb the churn. This story
 *   instantiates ONE reading of the nicene_christological_kernel; the
 *   same-substance sibling reading is a separate constraint story linked in
 *   network.affects_constraints, with its own epsilon, beneficiary set, and
 *   failure modes. Epsilon's referent here is the homoiousios-ordered
 *   arrangement itself, assessed by this reading's own lights — never the
 *   arrangement the sibling reading would build.
 *
 * KEY AGENTS:
 *   - eastern_episcopal_majority: Primary beneficiary (organized/constrained) — subscribes the formula, staffs the settlement, and receives conforming clergy and sees; also pays compliance costs when court drafts outrun conviction
 *   - regional_metropolitan_sees: Secondary beneficiary (institutional/constrained) — Alexandria, Antioch, and peer sees convert doctrinal flexibility into jurisdictional autonomy
 *   - imperial_administrative_apparatus: Agenda setter (institutional/arbitrage) — convokes councils, drafts and redrafts formulas, enforces by deposition and exile; exits any given wording at will
 *   - nicene_confessors: Primary target (moderate/identity_locked) — bear deposition and exile rather than repudiate the same-substance confession
 *   - anomoean_dissidents: Secondary target (moderate/identity_locked) — radical subordinationists crushed by the same middle formula
 *   - provincial_congregations: Diffuse target (powerless/trapped) — absorb clergy swaps, liturgical revision, and episodic violence
 *   - ecclesial_historians: Analytical observer (analytical/analytical) — reconstruct the formula sequence from acta, letters, and exile records
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_christological_kernel__homoiousios_reading, 0.55).
domain_priors:suppression_score(nicene_christological_kernel__homoiousios_reading, 0.4).
domain_priors:theater_ratio(nicene_christological_kernel__homoiousios_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_christological_kernel__homoiousios_reading, tangled_rope).
narrative_ontology:human_readable(nicene_christological_kernel__homoiousios_reading, "Homoiousios Settlement of the Nicene Christological Kernel").
narrative_ontology:topic_domain(nicene_christological_kernel__homoiousios_reading, "historical_theology/ecclesiastical_authority").

domain_priors:requires_active_enforcement(nicene_christological_kernel__homoiousios_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_christological_kernel__homoiousios_reading, 'af546aec-a583-4f85-9279-6555585bcf61').
narrative_ontology:cs_kernel_codification('af546aec-a583-4f85-9279-6555585bcf61', fixed_text).
narrative_ontology:cs_authority_grounding('af546aec-a583-4f85-9279-6555585bcf61', practice).
narrative_ontology:cs_interpretation_layer_present('af546aec-a583-4f85-9279-6555585bcf61').
narrative_ontology:cs_reading_relation('af546aec-a583-4f85-9279-6555585bcf61', nicene_christological_kernel__homoousios_reading, forecloses).
narrative_ontology:cs_axiom('af546aec-a583-4f85-9279-6555585bcf61', foundational, son_distinct_in_substance_from_father).
narrative_ontology:cs_axiom_status(son_distinct_in_substance_from_father, holdable).
narrative_ontology:cs_axiom_grounding('af546aec-a583-4f85-9279-6555585bcf61', son_distinct_in_substance_from_father, theological).
narrative_ontology:cs_axiom('af546aec-a583-4f85-9279-6555585bcf61', secondary, father_monarchical_priority_guards_monotheism).
narrative_ontology:cs_axiom_status(father_monarchical_priority_guards_monotheism, holdable).
narrative_ontology:cs_axiom_grounding('af546aec-a583-4f85-9279-6555585bcf61', father_monarchical_priority_guards_monotheism, theological).
narrative_ontology:cs_reference_frame('af546aec-a583-4f85-9279-6555585bcf61', scriptural_similarity_orthodoxy).
narrative_ontology:cs_drift_state('af546aec-a583-4f85-9279-6555585bcf61', constantinopolitan_settlement, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('af546aec-a583-4f85-9279-6555585bcf61', '').
narrative_ontology:cs_kernel_id(nicene_christological_kernel__homoiousios_reading, nicene_christological_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, eastern_episcopal_majority).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, regional_metropolitan_sees).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, imperial_administrative_apparatus).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, nicene_confessors).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, anomoean_dissidents).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, provincial_congregations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, eastern_episcopal_majority).
narrative_ontology:constraint_vindicates(nicene_christological_kernel__homoiousios_reading, monotheistic_clarity_doctrine).
narrative_ontology:constraint_vindicates(nicene_christological_kernel__homoiousios_reading, ontological_distinction_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The broad body of eastern bishops who subscribe the similarity formula in synod. Subscription keeps them in office and in communion; court-drafted formulas occasionally outrun their convictions, and refusal risks deposition. They receive installed clergy, filled sees, and control of communion networks, while paying obedience to each redraft.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, eastern_episcopal_majority, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoiousios_reading, eastern_episcopal_majority, payer).

% Great sees such as Alexandria, Antioch, and their peers. Doctrinal flexibility protects each see's inherited theological idiom and jurisdictional prerogatives from a single imposed vocabulary; in exchange they lend their networks to installing compliant clergy and hosting the synods that redraft the formula.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, regional_metropolitan_sees, beneficiary,
    institutional, generational, constrained, regional).

% The emperor and his court advisers. They convoke councils, draft and redraft subscribable formulas, and enforce them by deposing and exiling recalcitrant bishops, with troops installing replacements. What flows to them is governability: a church united enough to bless the regime, managed through a wording they can revise at will. Their commitment to any given formula is instrumental and reversible.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, imperial_administrative_apparatus, agenda_setter,
    institutional, generational, arbitrage, continental).

% Bishops and teachers bound to the same-substance confession of Nicaea. They refuse the similarity vocabulary as a repudiation of the received faith, and pay with deposition, exile, and replacement — Athanasius' successive returns from exile mark the rhythm. Signing would preserve their persons and destroy their office and self-understanding; they do not sign.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, nicene_confessors, payer,
    moderate, biographical, identity_locked, continental).

% Radical subordinationist teachers who deny the Son any substance-kinship with the Father. The middle formula condemns them as surely as it condemns the opposite extreme; they lose teaching posts and assemblies whenever the settlement's enforcement turns active, and their doctrine is too constitutive to soften for subscription.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, anomoean_dissidents, payer,
    moderate, biographical, identity_locked, continental).

% Local churches in cities and villages across the east. They receive whichever clergy the prevailing formula's machinery installs, absorb liturgical revision, and occasionally meet violence when factions contest a basilica. Moving to another congregation means leaving kin, land, and burial communities; they stay and comply.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, provincial_congregations, payer,
    powerless, biographical, trapped, local).

% Later analysts reconstructing the formula sequence from conciliar acta, exile correspondence, and court records. They hold no office under the arrangement and collect nothing from it; their seat is retrospective and evidentiary.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, ecclesial_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_christological_kernel__homoiousios_reading, eastern_episcopal_majority).
narrative_ontology:fixing_cost_class(nicene_christological_kernel__homoiousios_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives the eastern churches one subscribable doctrinal formula: a common confession affirming the Son's real divinity while permitting each jurisdiction to articulate the Father-Son relation in its own inherited grammar, enabling shared communion, mutual recognition of ordinations, and conciliar action.
% TRANSFER_FUNCTION: Moves doctrinal compliance, episcopal offices, and public legitimacy from dissenting teachers and their networks toward the subscribing episcopal majority and the imperial center; moves congregational allegiance along with whichever clergy the enforcement machinery installs.
% ABSENT_VOICES: Ordinary laity had no seat — their objection surface was riot and schism, not deliberation. Western bishops operated in a different linguistic tradition (Latin substantia) and were effectively outside the drafting rooms at Sirmium and Seleucia. Monastic and popular partisans of the exiled confessors spoke only through disturbance. The synthesizing theological generation formed after 361 was not yet in the room when the formulas were fixed.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, the eastern episcopate splits immediately between the same-substance confession and radical subordinationism with no middle formula to sign; the imperial government loses its instrument for managing the dispute and must either impose one side by force or tolerate open division; the metropolitan sees lose the jurisdictional shield that flexibility provided; congregations face immediate clergy turnover as each side reclaims churches.
% FOUNDING_PROBLEM: After Nicaea (325) a large majority of eastern bishops could not in conscience sign the same-substance formula, yet Arian subordinationism was unacceptable to nearly all of them; the churches needed a formula affirming the Son's divinity that the eastern episcopal mainstream could actually affirm, restoring usable unity.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the same-substance exiles (Athanasius' De Synodis cataloguing the formula sequence) attest the reception crisis was real even while insisting Nicaea had already solved it; western conciliar acta record the east-west breakdown; the pagan observer Ammianus Marcellinus attests the dispute's grip on the imperial court; Julian's rescripts mock the quarrel's intensity. No attesting source is limited to the settlement's beneficiaries.
narrative_ontology:disappearance_verdict(nicene_christological_kernel__homoiousios_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_christological_kernel__homoiousios_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_christological_kernel__homoiousios_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nicene_christological_kernel__homoiousios_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_christological_kernel__homoiousios_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_christological_kernel__homoiousios_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_christological_kernel__homoiousios_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_christological_kernel__homoiousios_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is authored from structure: the arrangement coordinates (a subscribable formula, common worship) and extracts asymmetrically (dissenters pay, court and episcopal majority collect), with enforcement doing load-bearing work — hence tangled_rope. The metrics are authored independently as description. Extractiveness 0.55 at interval end: the transfer (offices, legitimacy, compliance) is substantial but bounded — the formula left most bishops' actual teaching untouched. Suppression 0.40 at end-state reflects the enforcement machinery being dismantled as the settlement collapses; the series shows its mid-interval peak (0.74) under Valens. Suppression is authored as a raw structural property — the engine scales only extractiveness, by directionality and scope. Theater_ratio 0.52 at end: by 381 much of the settlement's activity was ceremonial subscription divorced from conviction, though mid-interval enforcement was functionally real. Accessibility_collapse 0.45: alternatives stayed livable — the same-substance confession survived underground and in the west, radical subordinationism regrouped, and Latin-speaking churches never accepted the Greek substance-vocabulary, so understanding the formula did not close the option space. Resistance 0.60: sustained — western episcopal defiance, riotous defense of deposed clergy in Antioch and Egypt, and the cross-see confessional coalition the exiles built, which shows the coalition power available even to nominally weak seats. All three tracked series share one time grid (358-381 at seven points) so no metric row borrows another's end-state.
 *
 * PERSPECTIVAL GAP:
 *   From the imperial seat the arrangement is a governance instrument: a formula adjustable enough to hold the eastern episcopate, costly only when enforcement outruns consent. From the metropolitan sees it is protective pluralism: flexibility shields local traditions from a single imposed vocabulary. From the episcopal majority it is both wage and leash — subscription buys position while court drafts discipline conscience. From the confessor seats it computes as something closer to pure extraction: the same enforcement that looks like moderation from the center lands as deposition, exile, and death on men whose refusal is identity-constitutive — Hosius of Corduba resisting under threat, Athanasius returning from five exiles. The identity-lock is ideological and professional at once: the confession constitutes both the self and the office; breaking it would preserve the man and destroy the bishop. If that frame broke — if signing similarity-language stopped meaning repudiating Nicaea — the confessor seat's experienced burden would drop sharply without any change in the enforcement machinery.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the episcopal majority, the metropolitan sees, and the imperial apparatus; victim declarations drive high directionality for the confessors, the dissidents, and the congregations. The episcopal majority carries a secondary payer role because subscription coercion touched its members directly — its derived position sits nearer symmetry than a pure collector's. Trapped and identity-locked exits push the three victim seats toward the full-target end: congregations cannot relocate their baptismal communities, and neither extreme party can sign the middle formula without self-repudiation. The imperial apparatus lists as beneficiary (it collects governability) but its arbitrage exit means it bears almost no cost from any particular wording — the derivation should read it near the beneficiary pole despite its agenda-setting role. Gains accrue demonstrably to the episcopal majority seat: conforming clergy, filled sees, controlled communion networks.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. Reading the arrangement as pure extraction ignores its real coordination yield: by 359 the formula was affirmable by an episcopal consensus no single-substance vocabulary could assemble in the east, and common worship depended on it. Reading it as pure coordination ignores the exile machinery and the crushed extremes. The tangled-rope reading holds both facts. On obsolescence: the founding problem — giving the eastern churches a formula they could actually sign — was live through the interval and was 'solved' not by this arrangement's success but by its displacement at Constantinople in 381, when the imperial patron switched sides and the machinery was turned to the sibling formula. The founding-problem status is therefore contested rather than dead: the beneficiaries attested its liveness to the end, while the same-substance party attested all along that the problem had been solved at Nicaea and only needed enforcement. Nothing here is a sunset arrangement — no clause ever contemplated transition; the settlement ended the way enforcement-backed settlements end, by losing the enforcer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the nicene_christological_kernel; how would instantiating the sibling homoousios_reading change the structural data?',
    'Author the sibling story and compare computed classifications; divergence in suppression and beneficiary sets confirms the readings are distinct constraints rather than one constraint viewed twice.',
    'If the sibling computes with materially higher suppression and a different beneficiary set, the kernel decomposes as modeled; if not, the two readings collapse into one constraint and this story''s epsilon is mis-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed nature of this story''s epsilon and structure.').

omega_variable(
    disagreement_location_substance_predicate,
    'Is the homoiousios/homoousios disagreement located in the substance predicate itself, or does it reduce to worship practice and theological grammar?',
    'Conceptual analysis of what each party''s ousia claims commit them to, tested against their own anathemas (each side condemned the other''s term).',
    'If relocated to practice, the readings become compatible and the foreclosure relation fails; this story''s reading_relations and the sibling''s would both need revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_substance_predicate, conceptual, 'Where the kernel disagreement is structurally located.').

omega_variable(
    homoean_enforcement_attribution,
    'How much of the measured suppression belongs to the homoiousios reading proper versus the homoean cousin formula (similarity-language without substance-commitment) that the court actually enforced after 359?',
    'Disentangle enforcement targets in the acta: homoiousian bishops (the Ancyra party) were themselves deposed by the homoean machine; separating their cases from homoean-enforced conformity reattributes the coercion.',
    'If the court''s machinery enforced the vaguer cousin formula, this reading''s suppression series overstates it and the mid-interval peak should drop.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(homoean_enforcement_attribution, empirical, 'Attribution of measured coercion between this reading and its homoean cousin.').

omega_variable(
    congregation_compliance_mechanism,
    'Was provincial-congregation compliance structural (no alternative clergy available) or internalized (the formula taught as unquestionable truth)?',
    'Post-settlement trajectory: where congregations reverted rapidly to the sibling formula once enforcement flipped in 380-381, compliance was structural; where attachment to similarity-language persisted, it was internalized.',
    'Internalized compliance raises the constraint''s effective suppression beyond the structural measure — the target carries it after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congregation_compliance_mechanism, empirical, 'Structural versus internalized compliance among congregations.').

omega_variable(
    counterfactual_unity_baseline,
    'Is ecclesiastical fragmentation a cost attributable to this reading, or was fragmentation inevitable under any formula given the dispute''s depth?',
    'Counterfactual comparison with the sibling reading''s actual enforcement record: if same-substance uniformity required comparable coercion, fragmentation-under-pluralism is not an incremental cost of this reading.',
    'If fragmentation was inevitable, this reading''s extractiveness estimate falls toward the coordination floor; if the sibling achieved cohesion at lower cost, the fragmentation premium is real.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_unity_baseline, conceptual, 'Whether the pluralism-fragmentation tradeoff counts as this reading''s extraction cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_christological_kernel__homoiousios_reading, 358, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t358, nicene_christological_kernel__homoiousios_reading, theater_ratio, 358, 0.16).
narrative_ontology:measurement_basis(nice_tr_t358, observed).
narrative_ontology:measurement(nice_tr_t361, nicene_christological_kernel__homoiousios_reading, theater_ratio, 361, 0.27).
narrative_ontology:measurement_basis(nice_tr_t361, observed).
narrative_ontology:measurement(nice_tr_t365, nicene_christological_kernel__homoiousios_reading, theater_ratio, 365, 0.33).
narrative_ontology:measurement_basis(nice_tr_t365, observed).
narrative_ontology:measurement(nice_tr_t369, nicene_christological_kernel__homoiousios_reading, theater_ratio, 369, 0.39).
narrative_ontology:measurement_basis(nice_tr_t369, observed).
narrative_ontology:measurement(nice_tr_t373, nicene_christological_kernel__homoiousios_reading, theater_ratio, 373, 0.43).
narrative_ontology:measurement_basis(nice_tr_t373, observed).
narrative_ontology:measurement(nice_tr_t377, nicene_christological_kernel__homoiousios_reading, theater_ratio, 377, 0.47).
narrative_ontology:measurement_basis(nice_tr_t377, observed).
narrative_ontology:measurement(nice_tr_t381, nicene_christological_kernel__homoiousios_reading, theater_ratio, 381, 0.52).
narrative_ontology:measurement_basis(nice_tr_t381, observed).

% Extraction over time
narrative_ontology:measurement(nice_be_t358, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 358, 0.4).
narrative_ontology:measurement_basis(nice_be_t358, observed).
narrative_ontology:measurement(nice_be_t361, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 361, 0.56).
narrative_ontology:measurement_basis(nice_be_t361, observed).
narrative_ontology:measurement(nice_be_t365, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 365, 0.64).
narrative_ontology:measurement_basis(nice_be_t365, observed).
narrative_ontology:measurement(nice_be_t369, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 369, 0.67).
narrative_ontology:measurement_basis(nice_be_t369, observed).
narrative_ontology:measurement(nice_be_t373, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 373, 0.65).
narrative_ontology:measurement_basis(nice_be_t373, observed).
narrative_ontology:measurement(nice_be_t377, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 377, 0.59).
narrative_ontology:measurement_basis(nice_be_t377, observed).
narrative_ontology:measurement(nice_be_t381, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 381, 0.55).
narrative_ontology:measurement_basis(nice_be_t381, observed).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t358, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 358, 0.32).
narrative_ontology:measurement_basis(nice_su_t358, observed).
narrative_ontology:measurement(nice_su_t361, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 361, 0.55).
narrative_ontology:measurement_basis(nice_su_t361, observed).
narrative_ontology:measurement(nice_su_t365, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 365, 0.68).
narrative_ontology:measurement_basis(nice_su_t365, observed).
narrative_ontology:measurement(nice_su_t369, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 369, 0.74).
narrative_ontology:measurement_basis(nice_su_t369, observed).
narrative_ontology:measurement(nice_su_t373, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 373, 0.71).
narrative_ontology:measurement_basis(nice_su_t373, observed).
narrative_ontology:measurement(nice_su_t377, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 377, 0.57).
narrative_ontology:measurement_basis(nice_su_t377, observed).
narrative_ontology:measurement(nice_su_t381, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 381, 0.4).
narrative_ontology:measurement_basis(nice_su_t381, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_christological_kernel__homoiousios_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoiousios_reading, nicene_christological_kernel__homoousios_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the Nicene christological kernel per the epsilon-invariance principle: 'the Son's relation to the Father' covers two structurally distinct arrangements. The homoousios reading builds a uniformity-enforcing settlement (high suppression, imperial-center and great-see beneficiaries, all subordinationists as victims); the homoiousios reading builds a pluralism-tolerant settlement (moderate extraction, episcopal-majority and regional-see beneficiaries, both extremes as victims). Their epsilon values differ; measuring one with the other's observable produces incoherence. Dependency runs from the sibling's history into this story's lifecycle: the same-substance reading's prior codification at Nicaea created the reception problem this reading answers, and its re-codification at Constantinople (381) terminated this arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
