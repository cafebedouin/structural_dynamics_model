% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__retributive_reading, []).

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
 *   constraint_id: state_killing_legitimacy__retributive_reading
 *   human_readable: Retributive Reading: Life-Forfeiture Through Proportional Desert (Lex Talionis)
 *   domain: criminal justice/political philosophy/legal theory
 *
 * SUMMARY:
 *   In retentionist jurisdictions the retributive doctrine holds that
 *   deliberate killing forfeits the killer's life-right and that the state's
 *   proportionate execution restores the moral equilibrium the murder broke.
 *   The constraint coordinates the community's response to homicide —
 *   bounding vengeance, fixing the penalty ceiling at the measure of the
 *   wrong — while taking from the condemned the totality of their remaining
 *   existence, an extraction the doctrine itself declares mandatory rather
 *   than regrettable. This file instantiates ONE reading of the
 *   state_killing_legitimacy kernel: the retributive reading, generated clean
 *   per the epsilon-invariance principle. The epsilon referent is the
 *   standing arrangement under contest — the practice of state execution of
 *   convicted murderers — assessed by this reading's own lights; the endorsed
 *   alternatives of sibling readings are not the referent and appear nowhere
 *   in the metrics. The claim and the metrics are independent authored facts:
 *   the claimed type is what this reading holds structurally true of its own
 *   arrangement; the metrics describe how the arrangement actually operates.
 *   Family membership: this is one of three readings of the kernel (with the
 *   deterrence and abolition readings, separate files); all three share the
 *   referent and author different epsilon and different victim/beneficiary
 *   structures over it — the abolition reading places no one in the target
 *   set and treats the executed as dignity-violated, the deterrence reading
 *   treats the executed as instrumental means and future-potential-victims as
 *   beneficiaries, this reading places the offender in the target set as
 *   morally deserving and the moral order's community as beneficiary. The
 *   readings are linked, not merged.
 *
 * KEY AGENTS:
 *   - convicted_murderers: Primary target (powerless/trapped) — bears the constraint's total cost; the desert determination defines their remaining existence as the material of punishment
 *   - murder_victims_kin: Primary beneficiary (moderate/constrained) — offered vindication as the proportionate answer to the wrong done to their kin
 *   - law_abiding_community: Secondary beneficiary (organized/generational) — consumes vindication as a public good; ratifies the desert standard electorally
 *   - retentionist_state_penal_authority: Agenda setter (institutional/generational) — enacts, adjudicates, and administers the desert machinery; institutionally identified with the proportionality commitment it pronounces
 *   - dissenting_victims_families: Excluded voice (moderate/constrained) — kin who refuse the offered vindication; the framework records their objection without weight
 *   - criminological_researchers: Analytical observer (analytical/global) — measures the regime's effects from outside the desert framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, 0.86).
domain_priors:suppression_score(state_killing_legitimacy__retributive_reading, 0.72).
domain_priors:theater_ratio(state_killing_legitimacy__retributive_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__retributive_reading, "Retributive Reading: Life-Forfeiture Through Proportional Desert (Lex Talionis)").
narrative_ontology:topic_domain(state_killing_legitimacy__retributive_reading, "criminal justice/political philosophy/legal theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__retributive_reading, '07d67eee-b2a3-4306-b0cf-9af755f02dc4').
narrative_ontology:cs_kernel_codification('07d67eee-b2a3-4306-b0cf-9af755f02dc4', formalized).
narrative_ontology:cs_authority_grounding('07d67eee-b2a3-4306-b0cf-9af755f02dc4', lineage).
narrative_ontology:cs_interpretation_layer_present('07d67eee-b2a3-4306-b0cf-9af755f02dc4').
narrative_ontology:cs_reading_relation('07d67eee-b2a3-4306-b0cf-9af755f02dc4', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('07d67eee-b2a3-4306-b0cf-9af755f02dc4', state_killing_legitimacy__abolition_reading, forecloses).
narrative_ontology:cs_axiom('07d67eee-b2a3-4306-b0cf-9af755f02dc4', foundational, grave_wrongdoing_forfeits_life_right).
narrative_ontology:cs_axiom_status(grave_wrongdoing_forfeits_life_right, holdable).
narrative_ontology:cs_axiom_grounding('07d67eee-b2a3-4306-b0cf-9af755f02dc4', grave_wrongdoing_forfeits_life_right, deontological).
narrative_ontology:cs_axiom('07d67eee-b2a3-4306-b0cf-9af755f02dc4', foundational, punishment_proportional_to_offense).
narrative_ontology:cs_axiom_status(punishment_proportional_to_offense, holdable).
narrative_ontology:cs_axiom_grounding('07d67eee-b2a3-4306-b0cf-9af755f02dc4', punishment_proportional_to_offense, deontological).
narrative_ontology:cs_reference_frame('07d67eee-b2a3-4306-b0cf-9af755f02dc4', proportional_desert_order).
narrative_ontology:cs_drift_state('07d67eee-b2a3-4306-b0cf-9af755f02dc4', contemporary_post_innocence_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('07d67eee-b2a3-4306-b0cf-9af755f02dc4', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__retributive_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, murder_victims_kin).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, law_abiding_community).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, convicted_murderers).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, lex_talionis_proportionality_principle).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, forfeiture_of_rights_by_grave_wrongdoing).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, state_desert_administration_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convicted of deliberate killing in a retentionist jurisdiction. The desert determination converts their remaining existence into the material of punishment: appeals, clemency petitions, and moratoria delay the sentence but constitute no exit from it, and no choice available to them alters the proportionality finding. They bear the entire cost the doctrine defines — the forfeiture itself.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, convicted_murderers, payer,
    powerless, biographical, trapped, national).

% Survive a family member's murder. The state's proportionate response is offered to them as vindication — the public declaration that the wrong done to their kin receives its measured answer. Whether the offer functions as closure varies by family; their standing in the process is as recipients of vindication, not as decision-makers, since sentencing judgment belongs to the state.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, murder_victims_kin, beneficiary,
    moderate, biographical, constrained, national).

% The political community whose shared moral order the execution is held to vindicate. It ratifies the desert standard through legislatures and referenda, absorbs the fiscal and institutional costs of the capital apparatus, and receives the maintained proposition that grave wrongs receive proportionate answers — a public good no member purchases separately.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, law_abiding_community, beneficiary,
    organized, generational, constrained, national).

% Legislatures enact capital statutes, courts find proportionality, corrections agencies carry out sentences. The authority administers the desert machinery end to end and could narrow or expand eligibility by statute and judicial interpretation, yet is bound by its own declared commitment: having pronounced the killing deserved, it must administer what it has pronounced.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, retentionist_state_penal_authority, agenda_setter,
    institutional, generational, constrained, national).

% Kin of murder victims who oppose the execution of their relatives' killers. The desert framework assigns them the same vindication it offers other kin and records their objection without weight — the proportionality calculus contains no slot for a beneficiary who refuses the benefit.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, dissenting_victims_families, excluded,
    moderate, biographical, constrained, national).

% Study execution regimes from outside the desert framework: deterrence findings, incapacitation effects, error rates, community outcomes. Their results enter public contest over the regime's effects but carry no standing inside its internal proportionality logic.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, criminological_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__retributive_reading, diffuse).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a settled, bounded, publicly administered answer to the community's obligations after deliberate killing: a proportionate state response replaces private vengeance cycles, fixes the penalty ceiling at the measure of the wrong, and maintains the moral order's commitment that grave wrongs receive proportionate answers.
% TRANSFER_FUNCTION: Moves the condemned's remaining life from the convicted murderer to the moral order administered by the state; secondarily moves vindication outward to the victims' kin and the law-abiding community as the declared yield of the proportionate answer.
% ABSENT_VOICES: The condemned themselves cannot participate in authoring the rules that define them as deserving targets. Among victims' kin, families who oppose execution are overridden by the state's desert determination — their objection is recorded but carries no weight in the proportionality calculus. Dissenting jurists within retentionist systems file minority opinions that do not alter the majority's desert finding.
% DISAPPEARANCE_RATIONALE: If the desert-forfeiture rule vanished overnight, retentionist sentencing structures, death-row apparatus, clemency processes, and victims'-rights politics would all lose their organizing principle: capital statutes would fall to amendment, condemned prisoners would be resentenced, and the community's answer to homicide would reorganize around whatever proportionality standard replaced it — the arrangement's dependents are numerous and named.
% FOUNDING_PROBLEM: Unbounded private vengeance after homicide — blood feud and retaliatory clan violence — together with the absence of a principled ceiling on punishment. Lex talionis was instituted to bound retaliation (a life for a life, not a war of annihilation) and later inherited by the state as the proportionality standard for the ultimate penalty.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on the transition from feud to public prosecution attests the founding problem and its partial solution from outside the beneficiary set; contemporary homicide data corroborates that the underlying problem — unbounded retaliatory violence absent a settled communal response — remains live. Abolitionist legal scholars, adversarial to the arrangement, nonetheless concede the historical coordination function while disputing its modern necessity.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__retributive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__retributive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_legitimacy__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__retributive_reading, 0.86, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__retributive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.86) because the constraint takes the whole of the target's remaining existence and, crucially, the desert framing removes every internal brake: if the killing is deserved, no proportionality review can discount the sentence below death, and the target class is defined by the constraint's own verdict machinery, leaving the extraction bounded by no external limit. Suppression (0.72, raw and unscaled — only extractiveness is scaled by directionality and scope) reflects the moral foreclosure of alternatives: within the reading, mercy, commutation, and life imprisonment are not competing options but category errors — injustices rather than policies — which collapses the practical choice space without any physical barrier. Accessibility collapse (0.78) is correspondingly high but short of natural-law levels, since societies demonstrably operate life-without-parole alternatives without logical contradiction; the collapse is normative-inside-the-frame, not physical. Resistance (0.60) is real and sustained: abolition movements, litigation, moratoria, and international pressure meet the constraint continuously in every retentionist jurisdiction. Theater ratio (0.30) is low-to-moderate: the core act is performed sincerely and often, but the contemporary gap between death sentences pronounced and executions carried out grows a performative layer — clemency ritual, symbolic final statements, deterrent rhetoric detached from frequency. The temporal series run on one shared grid (1976, 1985, 1995, 2005, 2015, 2024) with every tracked metric authored at every point. The arc they encode: post-1976 rebuild of the execution machinery, expansion through the late 1980s-1990s (broader eligibility, streamlined federal review peaking with 1996 review-limiting legislation), then contraction after 2000 (categorical exclusions for intellectually disabled and juvenile offenders, lethal-injection litigation, the innocence movement, moratoria, and a decade-long slide in death sentences and executions). Base extractiveness therefore peaks mid-interval and eases slightly as eligibility narrows; theater rises monotonically as the sentenced-but-not-executed gap widens; suppression_requirement traces the enforcement machinery's build-and-erosion arc. Note the deliberate divergence between the scalar suppression (0.72 — the structural foreclosure of alternatives, stable) and the suppression_requirement series (ending 0.58 — the active enforcement force, decaying): the moral logic retains its grip even as the machinery that executes it thins, and that divergence is the story's central drift signal.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the condemned's position the arrangement is total cost legitimized by other people's moral certainty — a proportionality finding they cannot appeal on its own terms, delivered by a process they cannot exit. From the kin seat it is an offered vindication whose acceptance varies family by family. From the community seat it is a public good consumed collectively. From the agenda-setter seat it is solemn administrative duty, and the seat carries an institutional identity fusion: the retentionist state has become the desert's administrator, its self-conception as guarantor of proportionate justice now constitutive of its penal authority, so that narrowing the practice reads internally as betraying the commitment rather than revising a policy — break that identity frame and the enforcement-decay arc accelerates sharply. Same-level divergence appears between the two kin seats: murder_victims_kin and dissenting_victims_families hold identical nominal position (family of a murder victim) and identical power and exit atoms, yet experience opposite valence, differentiated entirely by whether they accept the vindication the framework assigns them — and the framework has no procedural slot for a beneficiary who refuses the benefit, which is why the refusers sit in the excluded role. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The condemned are declared victims with trapped exit and no power: their derived directionality sits at the full-target end, and the engine's amplification for trapped targets plus national scope pushes their effective burden toward the maximum the arithmetic allows — appropriate, since the burden is everything. Murder_victims_kin and law_abiding_community are declared beneficiaries and derive low directionality; the community's benefit is a genuine public good (the maintained proportionality commitment) that no member captures privately. The retentionist_state_penal_authority derives near-symmetric directionality as administrator — it pays the machinery's fiscal and institutional costs while collecting authority from administering it. Two discipline points: first, the moral order itself is NOT listed as a beneficiary — it is a vindicated proposition (lex_talionis_proportionality_principle, forfeiture_of_rights_by_grave_wrongdoing, state_desert_administration_doctrine), and a vindicated proposition collects no rents; the real-world actors who benefit are the kin and the community. Second, the receipt surface: gain_flow is authored as diffuse as an affirmative checked claim — every named seat was examined and none captures the extraction's yield. The condemned's forfeited life produces vindicated desert, which disperses as a public good across the community and unevenly, family by family, among the kin (the vindication_actuality omega tracks whether even that dispersed yield materializes); the state's authority gain is instrumental to administration, not receipt of the extraction. fixing_cost is authored prohibitive from the seat that could fix it: for the retentionist authority, removing the constraint means repudiating the proportionality commitment its own identity rests on — unpayable within the reading's lights, and empirically durable, since repeal efforts in retentionist jurisdictions fail repeatedly against entrenched desert politics even where the mechanical cost of statutory amendment is trivial.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — bounding vengeance and fixing a principled penalty ceiling for homicide — remains live, so the arrangement is not mandatrophy-resolved and the R5 interview records status live against a world_rearranges verdict, a consistent pairing. The tangled_rope claim does preventive work in both directions. Against the mountain-mislabel: retributive rhetoric habitually presents desert as written into the moral fabric of the universe, a natural law no polity chose; authoring emerges_naturally as false, naming identifiable beneficiaries and a total-cost victim class, and declaring active enforcement keeps the arrangement in the constructed, contested register where its beneficiaries can be audited. Against the snare-mislabel: a purely cynical reading would call the coordination story cover for killing; but the coordination function is genuine and historically documented — lex talionis emerged precisely to bound feud — and the founding problem it solved has not died, so the extraction rides a real coordination structure rather than replacing one. The forward-looking risk the measurements encode: enforcement capacity is eroding while the desert claim persists rhetorically and the theater ratio climbs; if that divergence continues, the arrangement drifts toward theatrical maintenance of a death row that rarely executes — the piton trajectory — with the suppression_requirement series serving as the early-warning line. Nothing in the current data licenses declaring that drift resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_forfeiture,
    'This story instantiates the retributive reading of the state_killing_legitimacy kernel; the sibling readings relocate the entire victim/beneficiary structure — where exactly does the inter-reading disagreement bind?',
    'Structural comparison across the three family files: the disagreement locates in whether desert can forfeit the life-right. Adopting the abolition reading empties the target set (no one legitimately targetable; the executed become dignity-violated parties) and zeroes this reading''s beneficiary structure; adopting the deterrence reading recasts the executed as instrumental means and shifts beneficiaries to potential future victims.',
    'Cross-reading epsilon and classification values are incomparable unless joined on the kernel and reading tags; family-level analysis must aggregate per-reading stories rather than averaging their metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_forfeiture, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling adoption changes victim set, beneficiary set, and epsilon.').

omega_variable(
    verdict_tracks_guilt,
    'Does the applied capital verdict track actual guilt closely enough for the desert premise to bind on its own terms?',
    'Post-conviction exoneration rates, DNA-based re-examination of capital cases, and statistical estimation of the false-positive rate in death-sentenced populations.',
    'Systematic divergence collapses the reading from inside: executing the innocent is the retributive frame''s own paradigm of injustice, so sustained error would force either radical procedure revision or abandonment even among the reading''s adherents — no external critique required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verdict_tracks_guilt, empirical, 'Whether the desert determination''s input (the verdict) reliably identifies the deserving.').

omega_variable(
    proportionality_equivalence,
    'Is death for deliberate killing actually proportionate — does lex talionis equivalence hold given the asymmetries between murder and execution in certainty, premeditation, method, and suffering?',
    'Rigorous application of the reading''s own proportionality criterion: philosophical analysis of equivalence plus comparative evidence on the two events'' structures; the reading cannot dismiss the question without abandoning proportionality as its distinguishing method.',
    'If equivalence fails, the constraint is not proportional desert but disproportionate severity — the coordination story (a measured answer) detaches from the practice (an unmeasured one), degrading the beneficiary-side vindication claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_equivalence, conceptual, 'Whether the core proportional-desert equation survives strict application of its own standard.').

omega_variable(
    vindication_actuality,
    'Does execution actually produce the vindication and moral-order restoration that constitute the beneficiary-side yield, or is vindication asserted rather than delivered?',
    'Longitudinal studies of victims'' kin outcomes following execution versus commutation, and community-level survey work on whether the proportionality commitment is experienced as maintained.',
    'If vindication fails to materialize, the beneficiary ledger empties while the target-side cost remains total — within this reading''s own referent the arrangement then computes as extraction without coordination payoff, shifting its computed type toward the pure-extraction end of the family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vindication_actuality, empirical, 'Whether the declared gain of the arrangement is actually produced for the seats it is offered to.').

omega_variable(
    internalized_vs_enforced_consensus,
    'Is retentionist stability maintained by internalized desert conviction within the community, or by active enforcement against otherwise-drifting opinion?',
    'Opinion trajectories during enforcement moratoria and execution slowdowns: if support decays when enforcement pauses, the consensus is enforced; if it holds, the desert conviction is internalized.',
    'If internalized, the suppression_requirement series overstates the arrangement''s coercive dependence and enforcement decay alone will not produce abandonment; if enforced, the observed enforcement decay predicts eventual abandonment — resolving this omega determines which way the drift arc terminates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_enforced_consensus, empirical, 'Mechanism of persistence: internalized belief versus maintained coercive capacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__retributive_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(skl_rr_tr_t1976, state_killing_legitimacy__retributive_reading, theater_ratio, 1976, 0.12).
narrative_ontology:measurement_basis(skl_rr_tr_t1976, observed).
narrative_ontology:measurement(skl_rr_tr_t1985, state_killing_legitimacy__retributive_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement_basis(skl_rr_tr_t1985, observed).
narrative_ontology:measurement(skl_rr_tr_t1995, state_killing_legitimacy__retributive_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement_basis(skl_rr_tr_t1995, observed).
narrative_ontology:measurement(skl_rr_tr_t2005, state_killing_legitimacy__retributive_reading, theater_ratio, 2005, 0.24).
narrative_ontology:measurement_basis(skl_rr_tr_t2005, observed).
narrative_ontology:measurement(skl_rr_tr_t2015, state_killing_legitimacy__retributive_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement_basis(skl_rr_tr_t2015, observed).
narrative_ontology:measurement(skl_rr_tr_t2024, state_killing_legitimacy__retributive_reading, theater_ratio, 2024, 0.3).
narrative_ontology:measurement_basis(skl_rr_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(skl_rr_be_t1976, state_killing_legitimacy__retributive_reading, base_extractiveness, 1976, 0.8).
narrative_ontology:measurement_basis(skl_rr_be_t1976, observed).
narrative_ontology:measurement(skl_rr_be_t1985, state_killing_legitimacy__retributive_reading, base_extractiveness, 1985, 0.85).
narrative_ontology:measurement_basis(skl_rr_be_t1985, observed).
narrative_ontology:measurement(skl_rr_be_t1995, state_killing_legitimacy__retributive_reading, base_extractiveness, 1995, 0.91).
narrative_ontology:measurement_basis(skl_rr_be_t1995, observed).
narrative_ontology:measurement(skl_rr_be_t2005, state_killing_legitimacy__retributive_reading, base_extractiveness, 2005, 0.89).
narrative_ontology:measurement_basis(skl_rr_be_t2005, observed).
narrative_ontology:measurement(skl_rr_be_t2015, state_killing_legitimacy__retributive_reading, base_extractiveness, 2015, 0.87).
narrative_ontology:measurement_basis(skl_rr_be_t2015, observed).
narrative_ontology:measurement(skl_rr_be_t2024, state_killing_legitimacy__retributive_reading, base_extractiveness, 2024, 0.86).
narrative_ontology:measurement_basis(skl_rr_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(skl_rr_su_t1976, state_killing_legitimacy__retributive_reading, suppression_requirement, 1976, 0.55).
narrative_ontology:measurement_basis(skl_rr_su_t1976, observed).
narrative_ontology:measurement(skl_rr_su_t1985, state_killing_legitimacy__retributive_reading, suppression_requirement, 1985, 0.68).
narrative_ontology:measurement_basis(skl_rr_su_t1985, observed).
narrative_ontology:measurement(skl_rr_su_t1995, state_killing_legitimacy__retributive_reading, suppression_requirement, 1995, 0.78).
narrative_ontology:measurement_basis(skl_rr_su_t1995, observed).
narrative_ontology:measurement(skl_rr_su_t2005, state_killing_legitimacy__retributive_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement_basis(skl_rr_su_t2005, observed).
narrative_ontology:measurement(skl_rr_su_t2015, state_killing_legitimacy__retributive_reading, suppression_requirement, 2015, 0.62).
narrative_ontology:measurement_basis(skl_rr_su_t2015, observed).
narrative_ontology:measurement(skl_rr_su_t2024, state_killing_legitimacy__retributive_reading, suppression_requirement, 2024, 0.58).
narrative_ontology:measurement_basis(skl_rr_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__deterrence_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'capital punishment legitimacy' decomposes under the epsilon-invariance principle into three readings of the state_killing_legitimacy kernel, each a separate story with its own epsilon, victim set, and beneficiary set over the shared referent (the standing arrangement of state execution of convicted murderers). This file is the retributive reading (offender in the target set as morally deserving; community/moral order as beneficiary; high epsilon from desert-based legitimacy). The deterrence reading (execution as rational signal) and the abolition reading (categorical dignity violation) are siblings linked here; the retributive reading's lineage-grounded authority is frequently cited as upstream support for deterrence claims, while the abolition reading stands in logical opposition to this one. Every family member links the others via affects_constraints; no member averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
