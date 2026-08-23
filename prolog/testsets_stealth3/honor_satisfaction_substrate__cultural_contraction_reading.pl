% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__cultural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [EXTINCT - RESOLVED BY SUBSTRATE TRANSFORMATION]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__cultural_contraction_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: honor_satisfaction_substrate__cultural_contraction_reading
 *   human_readable: Honor-Satisfaction Substrate - Cultural Contraction Reading (Honor-to-Dignity Substrate Erosion)
 *   domain: historical sociology/cultural anthropology/legal history
 *
 * SUMMARY:
 *   For roughly four centuries (c. 1550-1900) the honor-satisfaction regime
 *   governed conflict among Europe's armed elites: an affront to honor
 *   imposed a bound duty to seek satisfaction through the duel, and refusal
 *   carried social death. This story instantiates the
 *   cultural_contraction_reading of the contested kernel
 *   honor_satisfaction_substrate: the regime's binding force never rested
 *   primarily on enforcement machinery - anti-dueling statutes proliferated
 *   for four centuries and failed - but on a culturally constituted
 *   conception of selfhood in which worth was defended standing. When North
 *   Atlantic cultures shifted from honor-selfhood to dignity-selfhood (worth
 *   as intrinsic and inalienable), the code's obligatoriness dissolved
 *   without any change in enforcement: dueling exited the thinkable
 *   action-set. The constraint is therefore modeled as substrate-emergent
 *   whose demise was erosion of its supporting substrate, not suppression.
 *   Epsilon's referent is the standing honor-satisfaction arrangement as this
 *   reading assesses it - never the dignity-culture order that replaced it.
 *   Sibling readings (practice_decline_reading,
 *   composite_overdetermined_reading) are separate constraints in separate
 *   files with their own epsilon values and victim structures; they are not
 *   averaged into this one. KEY AGENTS (by structural relationship): see
 *   key_agents.
 *
 * KEY AGENTS:
 *   - aristocratic_gentleman_class: Primary beneficiary and decentralized agenda-setter (powerful/identity_locked) - the code subsidized its status order; members administered it through peer sanction and seconds; exit meant social death
 *   - military_officer_corps: Dual-positioned beneficiary-payer (organized/identity_locked) - commissions rode on honor standing; the code was most compulsory here and most lethal
 *   - fencing_masters_dueling_trade: Secondary material beneficiary (moderate/mobile) - instruction, arms, seconds' fees, printed codes
 *   - coerced_challenged_parties: Primary payer among the governed (moderate/identity_locked) - compelled to the field under social-death threat
 *   - duel_fatalities_and_dependents: Ultimate payers (powerless/trapped) - the killed and those left to condone their deaths
 *   - excluded_subordinate_classes: Payers outside the frame (powerless/trapped) - bore the order's violence norms without access to its remedies
 *   - state_legal_authorities: Antagonistic institutional seat (institutional/constrained) - centuries of prohibition that failed to touch the constraint's operation
 *   - evangelical_reform_movements: Excluded voice (organized/constrained) - objected from outside the honor adjudication
 *   - historical_sociology_observers: Analytical observer (analytical/analytical) - reconstructs the substrate transformation from the full arc
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__cultural_contraction_reading, 0.61).
domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, 0.55).
domain_priors:theater_ratio(honor_satisfaction_substrate__cultural_contraction_reading, 0.17).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 0.17).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, 0.84).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__cultural_contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_substrate__cultural_contraction_reading, "Honor-Satisfaction Substrate - Cultural Contraction Reading (Honor-to-Dignity Substrate Erosion)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__cultural_contraction_reading, "historical sociology/cultural anthropology/legal history").

domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__cultural_contraction_reading, '23fb5993-4b13-445c-9984-f303247d01d7').
narrative_ontology:cs_kernel_codification('23fb5993-4b13-445c-9984-f303247d01d7', implicit).
narrative_ontology:cs_authority_grounding('23fb5993-4b13-445c-9984-f303247d01d7', practice).
narrative_ontology:cs_interpretation_layer_present('23fb5993-4b13-445c-9984-f303247d01d7').
narrative_ontology:cs_reading_relation('23fb5993-4b13-445c-9984-f303247d01d7', honor_satisfaction_substrate__practice_decline_reading, forecloses).
narrative_ontology:cs_reading_relation('23fb5993-4b13-445c-9984-f303247d01d7', honor_satisfaction_substrate__composite_overdetermined_reading, coexists_with).
narrative_ontology:cs_axiom('23fb5993-4b13-445c-9984-f303247d01d7', foundational, normativity_is_substrate_bound).
narrative_ontology:cs_axiom_status(normativity_is_substrate_bound, holdable).
narrative_ontology:cs_axiom_grounding('23fb5993-4b13-445c-9984-f303247d01d7', normativity_is_substrate_bound, empirically_contingent).
narrative_ontology:cs_axiom('23fb5993-4b13-445c-9984-f303247d01d7', foundational, unthinkability_not_prohibition_ends_practice).
narrative_ontology:cs_axiom_status(unthinkability_not_prohibition_ends_practice, holdable).
narrative_ontology:cs_axiom_grounding('23fb5993-4b13-445c-9984-f303247d01d7', unthinkability_not_prohibition_ends_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('23fb5993-4b13-445c-9984-f303247d01d7', honor_culture_constitutive_order).
narrative_ontology:cs_drift_state('23fb5993-4b13-445c-9984-f303247d01d7', dignity_culture_transition, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('23fb5993-4b13-445c-9984-f303247d01d7', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, aristocratic_gentleman_class).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, military_officer_corps).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, fencing_masters_dueling_trade).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, duel_fatalities_and_dependents).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, coerced_challenged_parties).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, excluded_subordinate_classes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, military_officer_corps).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, state_legal_authorities).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__cultural_contraction_reading, point_of_honor_doctrine).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__cultural_contraction_reading, reputation_as_security_collateral).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hereditary elite whose standing, marriage prospects, and political access rested on reputation treated as capital. The code gave members a settled procedure for defending that capital and a bright boundary against commoners. Members administered the code themselves - acting as seconds, sitting on informal tribunals, and shunning refusers. Leaving the code meant forfeiting the identity that organized their lives, so none left individually; the class abandoned it only when the underlying conception of worth changed beneath them.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, aristocratic_gentleman_class, beneficiary,
    powerful, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__cultural_contraction_reading, aristocratic_gentleman_class, agenda_setter).

% Commissioned officers in armies where honor standing gated advancement and refusal to give satisfaction could end a career through forced resignation or social expulsion. The code was most compulsory here - regimental opinion and honor courts pressed men to the field - and most lethal: officers died in numbers disproportionate to their share of the population. The same men drew rank and promotion from the order they died under.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, military_officer_corps, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__cultural_contraction_reading, military_officer_corps, payer).

% Swordsmen, arms dealers, and publishers who sold instruction, weapons, seconds' services, and printed codes of practice. Income tracked the volume of affairs of honor. Skills transferred to teaching and sport, so exit was feasible; most opposed the regime's decline quietly while serving it professionally.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, fencing_masters_dueling_trade, beneficiary,
    moderate, immediate, mobile, regional).

% Gentlemen who received challenges or committed affronts and faced the choice between the field and social death. Many went reluctantly; diaries and correspondence record dread, attempts to negotiate apology formulas, and relief when seconds engineered bloodless outcomes. Their standing - livelihood, marriage, company - was hostage to compliance.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, coerced_challenged_parties, payer,
    moderate, biographical, identity_locked, national).

% The men killed in affairs of honor and the widows and orphans left behind. Families were pressured to condone the deaths as honorable; some dependents publicly forgave and thereby scandalized the code, others conformed. They bore the regime's ultimate cost and had no standing in its adjudication.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, duel_fatalities_and_dependents, payer,
    powerless, biographical, trapped, local).

% Servants, laborers, tradesmen, and colonized subjects who lived under the elite's armed habits without access to the satisfaction machinery: an affront from a gentleman to an inferior demanded no satisfaction, while inferiors who took their own redress faced the criminal law. They bore the order's violence norms and its impunity asymmetry without membership or voice.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, excluded_subordinate_classes, payer,
    powerless, generational, trapped, continental).

% Monarchs, magistrates, and legislators who issued repeated prohibitions - edicts, statutes, articles of war - across four centuries and largely failed to stop the practice; prosecutions foundered on jury sympathy, witness silence, and the complicity of officers drawn from the same class. The state bore a sovereignty cost: private, adjudicated violence operating inside its jurisdiction.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, state_legal_authorities, payer,
    institutional, civilizational, constrained, national).

% Religious and utilitarian reformers who condemned the code as sin and superstition, founded anti-dueling societies, and mobilized press opinion. Their objections carried no standing inside the honor adjudication, which admitted only principals, seconds, and peers; their leverage grew only as the surrounding culture turned.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, evangelical_reform_movements, excluded,
    organized, generational, constrained, national).

% Later scholars reconstructing the transformation from legal records, duel statistics, correspondence, and conduct literature; they observe the full arc, including the endgame in which the code dissolved without being defeated.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, historical_sociology_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__cultural_contraction_reading, aristocratic_gentleman_class).
narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__cultural_contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channeled disputes among armed equals into a bounded, ritualized encounter with agreed rules and seconds, replacing vendetta and ambush with a single regulated exchange; simultaneously sorted the elite from commoners by reserving the procedure to those with standing to give and demand satisfaction.
% TRANSFER_FUNCTION: Moved life-risk and lives from challenged and challenging gentlemen onto the field of honor; moved deference and advancement to compliant members; moved fees to fencing masters, arms sellers, and code publishers; moved the authority to define acceptable conduct from individuals and courts to the peer-administered code.
% ABSENT_VOICES: The killed (their testimony is structurally absent from every affair), the widows and orphans pressured to condone, subordinate classes with no access to the machinery, and religious dissenters - all outside the honor adjudication, which admitted only principals, seconds, and peers. Their objections register only in criminal records, pamphlets, and later scholarship.
% DISAPPEARANCE_RATIONALE: The regime is extinct: it dissolved with the honor-selfhood substrate in the late nineteenth century, and nothing in the present world depends on its operation. Had it vanished at maturity (c. 1760) the world would have rearranged - status dispute would have reverted to vendetta or litigation, officer advancement would have lost its screening ritual, the fencing trade would have collapsed - but that dependency died with the substrate. Today its absence is fully priced in.
% FOUNDING_PROBLEM: In an era before state violence-monopoly reached the gentry and before courts offered credible, timely redress between armed equals, personal reputation was the primary security collateral: an affront left unanswered invited further predation. The arrangement answered how armed equals could deter insult and aggression without descending into endless feud.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: legal historians document the rise of court credibility and policing that made private satisfaction superfluous; comparative anthropology shows honor complexes recurring precisely where state reach fails and receding where dignity-selfhood spreads; contemporary evangelical and liberal reformers testified that courts had rendered the code archaic. The gentleman class itself attested the opposite - that honor remained live - which is why its testimony is excluded here.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__cultural_contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__cultural_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__cultural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 0.61, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_substrate__cultural_contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base scalars characterize the regime at maturity (c. 1690-1760), which is the epsilon referent this reading assesses; the measurement series traces the full lifecycle to dissolution on one shared seven-point grid, so every tracked metric is authored at every examined time point. Extractiveness 0.61: the regime took lives and coerced lethal risk, but within a governed class that broadly endorsed the order and drew standing from it - extraction and subsidy ran through the same structure. Suppression 0.55 is authored as a raw structural property, unscaled by power or scope: the operative force was internalized identity-lock plus diffuse peer sanction, not an enforcement apparatus - the regime persisted through centuries of active legal prohibition aimed against it, which is this reading's central evidence that external enforcement was not load-bearing. Theater 0.17 at maturity: ritual was functionally loaded; the series shows theater_ratio rising monotonically to 0.60 as conviction thinned - form outliving substance, Goodhart drift - while suppression_requirement falls from 0.55 to 0.12 (enforcement decay, the trajectory this story specifically tracks) and base_extractiveness arcs up to 0.64 then down to 0.34 as the governed population thinned. Accessibility_collapse 0.84: inside the honor frame, alternatives to satisfaction collapsed almost completely - refusal was not a live option. Resistance 0.28: dissent came disproportionately from outside the governed class (evangelicals, utilitarian reformers); the governed policed one another. requires_active_enforcement is authored false: peer sanction was expressive of the substrate rather than a necessary support - remove the sanction and the substrate regenerates it; remove the substrate and no sanction quantity holds the code, which is what the record shows. Fixing-cost evidence: individual exit meant social death, collective abolition required a substrate change no actor controlled, and state prohibition failed for four centuries - fixing was prohibitive for every candidate fixer. Identity-coordination caveat: the complexity leeway this coordination type carries must not excuse the extraction concentrated on excluded subordinate classes at continental scope; the coupling here runs from a powerless, wide-scope seat, which is flagged rather than absorbed.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute divergently. From the gentleman-class seat the regime presents as constitutive order - effective extraction should read low there, subsidy-dominated. From the coerced-challenged and fatality-dependent seats the same structure operates as lethal compulsion with no exit. The officer seat straddles: maximal benefit (advancement) and maximal compulsion (no refusal) in the same biography. The excluded-subordinate seat experiences a regime it was never party to governing - its violence norms without its remedies. The analytical seat sees the whole arc, including the endgame in which the class converted to dignity-selfhood and the code evaporated without ever being defeated. These divergences are computed by the engine from the structural data; the authored mountain claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The gentleman class is declared beneficiary and carries identity_locked exit: the derivation could misread identity-lock as target-marking and push d upward, so an override sets d=0.15 - the lock bound members to a structure that subsidized their standing; their duel-risk was incurred inside a frame that returned rank. Military officers are dual-positioned (beneficiary and payer): override d=0.45, near symmetric and leaning payer, because compulsion was strongest in armies (regimental opinion, honor courts, resignation-for-refusal) while advancement benefits flowed to the same men. Fencing trades sit near the beneficiary pole on mobile exit and direct fees. Coerced challenged parties derive high d from the victim declaration plus identity-lock. Duel fatalities and dependents and excluded subordinate classes derive near-full-target d: they bore the regime's costs with no access to its benefits, the excluded classes being the sharpest case - subject to the order's impunity asymmetry while barred from its satisfaction machinery. State authorities derive moderate-to-high d as payers of sovereignty cost. Scope amplification applies through the class's continental footprint; suppression passes through unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   This is the clean case the zombie flag exists to distinguish from. The founding problem - private security of reputation where state violence-monopoly and credible courts are absent - is dead, and the arrangement died WITH its mandate: no theatrical maintenance, no inertial administration, no concentrated capturer keeping the form alive. The R5 pairing (status=dead, verdict=world_unchanged) is therefore consistent, not mismatched: a constraint can have arranged the world utterly at maturity and leave nothing dependent after genuine obsolescence. Contrast the degraded-inertial shape (mandate dead, arrangement persisting on performance) - this regime left at most bounded residuals (sealed corporate fencing cults) whose persistence is itself carried as an open question. The analysis prevents the inverse mislabel too: reading the regime's four-century persistence under legal prohibition as enforcement-success would misclassify a substrate-carried order as a suppressed coordination mechanism; this reading's claim is that no enforcement quantity explains either its persistence or its death.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substrate_naturality_ambiguity,
    'Is the honor-satisfaction regime a substrate-emergent formation of pre-state elite order (mountain-profile), or a constructed class-interest regime whose naturality is a false summit?',
    'Comparative anthropological record: honor complexes recur across unconnected pastoral and pre-state societies (Caucasus, Mediterranean, American backcountry) wherever state violence-monopoly and credible courts are absent, and recede where dignity-selfhood spreads; weighed against documented elite codification and interest-serving moments (codes duello, army honor courts).',
    'If constructed, the mountain claim fails and the false-summit chain reclassifies toward tangled_rope with the gentleman class as capturing beneficiary; if emergent, the class benefit is incidental to a substrate-carried order.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_naturality_ambiguity, empirical, 'Natural formation versus constructed class regime.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (cultural_contraction_reading) of the kernel honor_satisfaction_substrate - how would instantiating a sibling reading change the structural classification?',
    'Generate the sibling files and compare computed types across the family: practice_decline_reading keeps the substrate alive and attributes decline to exogenous enforcement (lower terminal epsilon, suppression-centered profile); composite_overdetermined_reading splits causality across non-independent endogenous and exogenous pathways (hybrid profile).',
    'Classification is indexical to the reading: mountain-erosion here, plausibly a suppressed coordination profile under practice_decline, hybrid under composite. Cross-reading comparison is the measurement; no reading-neutral classification of this kernel exists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Reading-indexical classification of the honor kernel; sibling readings are separate constraints in separate files.').

omega_variable(
    endogeneity_identification,
    'Can substrate transformation be causally separated from the simultaneous growth of state capacity (violence-monopoly, credible courts), given that the two covary across the North Atlantic?',
    'Natural experiments where the covariation breaks: honor-culture persistence inside strong states (the American South puzzle), dignity shifts in weak-state regions, and diaspora populations carrying honor norms into dignity jurisdictions.',
    'If inseparable, this reading collapses toward composite_overdetermined_reading; if separable cases exist, substrate causation is identified independently of state-building.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogeneity_identification, empirical, 'Substrate shift versus state-building confound.').

omega_variable(
    suppression_internalization_split,
    'Was the regime''s hold on participants structural (peer sanction, army pressure, economic dependency on standing) or internalized (identity fusion with honor-selfhood)?',
    'Post-substrate trajectory: when peer sanction relaxed in the late nineteenth century, compliance collapsed within a generation, indicating internalization carried the load; army contexts where coercion outlasted conviction isolate the structural remainder.',
    'If predominantly internalized, the authored suppression understates the constraint''s effective grip at maturity and supports the mountain profile; if structural, the regime sits closer to an enforced order.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized compliance mechanism.').

omega_variable(
    mensur_residual_counterexample,
    'German academic fencing persisted into the dignity era inside closed student corporations - does this bounded survivor falsify the unthinkability mechanism?',
    'Determine whether the survivor''s persistence tracks surviving honor-substrate in sealed corporate identities or transformation into sport-like practice detached from any satisfaction obligation; compare against the general population''s action-set.',
    'If substrate survival in micro-climates, the reading holds with a scope qualifier; if the practice persisted while the satisfaction obligation died, the mechanism claim needs revision from unthinkability to obligation-dissolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mensur_residual_counterexample, empirical, 'Bounded residual testing the unthinkability axiom.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__cultural_contraction_reading, 1550, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1550, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1550, 0.1).
narrative_ontology:measurement_basis(hono_tr_t1550, observed).
narrative_ontology:measurement(hono_tr_t1620, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1620, 0.12).
narrative_ontology:measurement_basis(hono_tr_t1620, observed).
narrative_ontology:measurement(hono_tr_t1690, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1690, 0.15).
narrative_ontology:measurement_basis(hono_tr_t1690, observed).
narrative_ontology:measurement(hono_tr_t1760, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1760, 0.2).
narrative_ontology:measurement_basis(hono_tr_t1760, observed).
narrative_ontology:measurement(hono_tr_t1815, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1815, 0.3).
narrative_ontology:measurement_basis(hono_tr_t1815, observed).
narrative_ontology:measurement(hono_tr_t1860, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1860, 0.45).
narrative_ontology:measurement_basis(hono_tr_t1860, observed).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1900, 0.6).
narrative_ontology:measurement_basis(hono_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t1550, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1550, 0.42).
narrative_ontology:measurement_basis(hono_be_t1550, observed).
narrative_ontology:measurement(hono_be_t1620, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1620, 0.5).
narrative_ontology:measurement_basis(hono_be_t1620, observed).
narrative_ontology:measurement(hono_be_t1690, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1690, 0.58).
narrative_ontology:measurement_basis(hono_be_t1690, observed).
narrative_ontology:measurement(hono_be_t1760, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1760, 0.64).
narrative_ontology:measurement_basis(hono_be_t1760, observed).
narrative_ontology:measurement(hono_be_t1815, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1815, 0.58).
narrative_ontology:measurement_basis(hono_be_t1815, observed).
narrative_ontology:measurement(hono_be_t1860, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1860, 0.48).
narrative_ontology:measurement_basis(hono_be_t1860, observed).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1900, 0.34).
narrative_ontology:measurement_basis(hono_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1550, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1550, 0.5).
narrative_ontology:measurement_basis(hono_su_t1550, observed).
narrative_ontology:measurement(hono_su_t1620, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1620, 0.52).
narrative_ontology:measurement_basis(hono_su_t1620, observed).
narrative_ontology:measurement(hono_su_t1690, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1690, 0.55).
narrative_ontology:measurement_basis(hono_su_t1690, observed).
narrative_ontology:measurement(hono_su_t1760, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1760, 0.55).
narrative_ontology:measurement_basis(hono_su_t1760, observed).
narrative_ontology:measurement(hono_su_t1815, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1815, 0.42).
narrative_ontology:measurement_basis(hono_su_t1815, observed).
narrative_ontology:measurement(hono_su_t1860, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1860, 0.28).
narrative_ontology:measurement_basis(hono_su_t1860, observed).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1900, 0.12).
narrative_ontology:measurement_basis(hono_su_t1900, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__cultural_contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the decline of dueling' decomposes into three readings of the kernel honor_satisfaction_substrate, each with its own epsilon and victim structure. This file (cultural_contraction_reading) authors epsilon for the standing honor-satisfaction arrangement as assessed from the substrate-transformation account: the code's obligatoriness was substrate-carried and dissolved with honor-selfhood. practice_decline_reading authors the same kernel from the enforcement account (substrate persists; lower epsilon, suppression-centered). composite_overdetermined_reading authors the joint-causation account. The links run from this file to both siblings because the enforcement-failure record (four centuries of ineffective prohibition) is the evidentiary input each sibling must accommodate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_substrate__cultural_contraction_reading, powerful, 0.15).
constraint_indexing:directionality_override(honor_satisfaction_substrate__cultural_contraction_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
