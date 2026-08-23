% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__feudal_prerogative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__feudal_prerogative_reading, []).

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
 *   constraint_id: magna_carta_clause_39__feudal_prerogative_reading
 *   human_readable: Clause 39 as Feudal Privilege: Rank-Bound Procedural Guarantee within the Hierarchical Order
 *   domain: constitutional/legal_history/political_theory
 *
 * SUMMARY:
 *   At Runnymede in June 1215, an armed baronial confederation extracted from
 *   King John a promise that no free man would be seized, imprisoned,
 *   disseised, outlawed, exiled, or destroyed except by the lawful judgment
 *   of his equals or by the law of the land. Read within its own century, the
 *   promise is narrow by construction: 'free man' names a legal class —
 *   barons, knights, and the free tenantry — not the realm's population, the
 *   great majority of whom were unfree and outside the words entirely. The
 *   clause was repudiated within weeks, annulled by the pope, and regranted
 *   in 1216, 1217, and 1225; the coercive enforcement committee of 1215 was
 *   dropped after 1217 and the guarantee settled into routine judicial
 *   administration, punctuated by renewal crises (1258, 1297) in which the
 *   barons re-extracted the promise against fiscal pressure. Across the
 *   interval the arrangement does two things at once: it solves the
 *   crown-elite collective-action problem that had produced civil war, and it
 *   distributes procedural security as a class privilege whose price — the
 *   unfree majority's continued exclusion from any procedural standing — is
 *   paid silently. The claim and the metrics are authored independently: the
 *   tangled_rope claim states the structure believed true of this reading;
 *   the metric values state its operation as descriptively assessed, and
 *   where the engine's computed type diverges from the claim, that divergence
 *   is the datum.
 *
 * KEY AGENTS:
 *   - the_crown: principal payer and agenda-setter (institutional/constrained) — cedes bounded prerogative, regrants the promise four times, administers the law it promised to obey
 *   - baronage_and_knights: primary beneficiary (organized/constrained) — the protected class; extracted the grant and enforced it 1215-1217 by distraining power
 *   - lesser_free_tenants: secondary beneficiary (moderate/constrained) — count as 'free men,' ride on protection won and renewed by the baronage
 *   - unfree_peasantry: payer without voice (powerless/trapped) — the majority; bear the settlement's cost through exclusion from the guarantee
 *   - free_women: excluded (moderate/constrained) — hold free tenures but stand outside the guarantee's wording
 *   - monastic_chroniclers: analytical observer (analytical/analytical) — record the whole arc from outside the compact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__feudal_prerogative_reading, 0.31).
domain_priors:suppression_score(magna_carta_clause_39__feudal_prerogative_reading, 0.3).
domain_priors:theater_ratio(magna_carta_clause_39__feudal_prerogative_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__feudal_prerogative_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__feudal_prerogative_reading, "Clause 39 as Feudal Privilege: Rank-Bound Procedural Guarantee within the Hierarchical Order").
narrative_ontology:topic_domain(magna_carta_clause_39__feudal_prerogative_reading, "constitutional/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__feudal_prerogative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__feudal_prerogative_reading, 'b1b4e0dc-30ec-428f-a3f1-43d956682368').
narrative_ontology:cs_kernel_codification('b1b4e0dc-30ec-428f-a3f1-43d956682368', fixed_text).
narrative_ontology:cs_authority_grounding('b1b4e0dc-30ec-428f-a3f1-43d956682368', lineage).
narrative_ontology:cs_interpretation_layer_present('b1b4e0dc-30ec-428f-a3f1-43d956682368').
narrative_ontology:cs_reading_relation('b1b4e0dc-30ec-428f-a3f1-43d956682368', magna_carta_clause_39__liberal_due_process_reading, influences).
narrative_ontology:cs_reading_relation('b1b4e0dc-30ec-428f-a3f1-43d956682368', magna_carta_clause_39__originalist_limitation_reading, coexists_with).
narrative_ontology:cs_axiom('b1b4e0dc-30ec-428f-a3f1-43d956682368', foundational, procedural_rights_are_rank_bound).
narrative_ontology:cs_axiom_status(procedural_rights_are_rank_bound, holdable).
narrative_ontology:cs_axiom_grounding('b1b4e0dc-30ec-428f-a3f1-43d956682368', procedural_rights_are_rank_bound, conventional).
narrative_ontology:cs_axiom('b1b4e0dc-30ec-428f-a3f1-43d956682368', foundational, prerogative_channeled_not_displaced).
narrative_ontology:cs_axiom_status(prerogative_channeled_not_displaced, holdable).
narrative_ontology:cs_axiom_grounding('b1b4e0dc-30ec-428f-a3f1-43d956682368', prerogative_channeled_not_displaced, conventional).
narrative_ontology:cs_axiom('b1b4e0dc-30ec-428f-a3f1-43d956682368', secondary, judgment_by_equals_is_class_court).
narrative_ontology:cs_axiom_status(judgment_by_equals_is_class_court, holdable).
narrative_ontology:cs_axiom_grounding('b1b4e0dc-30ec-428f-a3f1-43d956682368', judgment_by_equals_is_class_court, conventional).
narrative_ontology:cs_reference_frame('b1b4e0dc-30ec-428f-a3f1-43d956682368', feudal_compact_bounded_prerogative).
narrative_ontology:cs_drift_state('b1b4e0dc-30ec-428f-a3f1-43d956682368', early_seventeenth_century_common_law_revival, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b1b4e0dc-30ec-428f-a3f1-43d956682368', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, baronage_and_knights).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, lesser_free_tenants).
narrative_ontology:constraint_victim(magna_carta_clause_39__feudal_prerogative_reading, the_crown).
narrative_ontology:constraint_victim(magna_carta_clause_39__feudal_prerogative_reading, unfree_peasantry).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__feudal_prerogative_reading, peer_judgment_procedure).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__feudal_prerogative_reading, law_of_the_land_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the realm's prerogative — seizure of lands, imprisonment, wardships and marriages, fiscal exactions. Under armed baronial pressure in 1215 it promised to proceed against a free man's person or holdings only by judgment of his equals or the law of the land, and it regranted that promise in 1216, 1217, and 1225, confirming it again in 1297 under threat of withheld taxation. Its own courts administer the promise day to day. It cannot walk out: repudiation in 1215-1217 cost it the realm's obedience and nearly the throne. What it ceded is bounded — jurisdiction over the unfree, wardship, and most fiscal power remain its own, and the settlement preserved the dynasty through the crisis that produced it.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, the_crown, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__feudal_prerogative_reading, the_crown, agenda_setter).

% Holds estates, honors, and military tenures; extracted the promise at Runnymede and enforced it in 1215-1217 through a committee of twenty-five empowered to distrain the king's castles if he reneged. Its persons, heirs, and lands are what the guarantee covers. Exit means abandoning English land and lineage; a few went to Ireland or the continent at heavy cost. It returns periodically to renew the promise when it frays — 1258, 1264, 1297.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, baronage_and_knights, beneficiary,
    organized, generational, constrained, national).

% Knights, sokemen, and some townsmen who count as 'free men' under the words. They receive the same shield against seizure, imprisonment, and disseisin without the baronage's collective muscle; they ride on protection won and renewed by others. Leaving free tenure means dropping into the statuses below it.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, lesser_free_tenants, beneficiary,
    moderate, biographical, constrained, national).

% Villeins and serfs, the majority of the population. They owe labor services and dues, answer in their lords' courts, and fall outside the words 'free man.' The Runnymede settlement was made over their heads: the peace it purchased confirmed the manorial order in which they hold no procedural standing against lord or king. A fugitive could be reclaimed; a year and a day in a chartered town was the narrow, costly leak. No seat at Runnymede, no voice in any reissue.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, unfree_peasantry, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__feudal_prerogative_reading, unfree_peasantry, excluded).

% Widows and heiresses holding free tenures. The charter's opening clauses shield their dower and their consent to remarriage, but the great procedural promise is worded for 'free men,' and their access to it runs through husbands, guardians, and lords. They would read the promise as theirs; the text and the courts do not reliably hand it to them.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, free_women, excluded,
    moderate, biographical, constrained, national).

% Monastic and court-adjacent writers — Matthew Paris at St Albans, the Dunstable annalist — who record the grants, the repudiations, the reissues, the ceremonies, and the kings' evasions. They watch the whole arc from outside the compact. They write; they do not decide.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, monastic_chroniclers, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__feudal_prerogative_reading, baronage_and_knights).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__feudal_prerogative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the crown-elite collective-action problem that produced the 1215 crisis: converts the king's personal discretion over elite persons and lands into a fixed procedure — judgment by one's equals or the law of the land — so that the feudal compact holds without each baron defending his estate by private war, and elite succession and property transfer proceed on predictable terms.
% TRANSFER_FUNCTION: Moves procedural security from a royal discretion-good to a class entitlement: the crown cedes a bounded slice of prerogative (seizure, imprisonment, disseisin, outlawry as against free men) to the baronage and lesser free tenants, while the unfree majority's standing passes unchanged — the settlement's peace is financed by their continued exclusion from the guarantee.
% ABSENT_VOICES: The villeins and serfs — the majority of the realm — had no seat at Runnymede, no voice in any reissue, and no standing under 'free man'; they would object that protection is rationed by rank. Free women, though the charter's early clauses touch widowhood, stand outside the clause's guarantee in practice. Their absence is not incidental to this reading — it is its content.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would return elite persons and lands to royal discretion. The 1215-1217 sequence is the natural experiment: John's repudiation brought papal annulment, civil war, and a French prince's invitation to the throne. Baronial property security, the courts' procedure for elite grievance, and the compact's peace all depend on the clause; the arrangement would rearrange within a season.
% FOUNDING_PROBLEM: King John's arbitrary disseisin of baronial lands, imprisonment of elite opponents, mercenary exactions, and scutage taken without consent made elite persons and property insecure against the king's will; the barons sought a procedural guarantee — judgment of equals or law of the land — as the price of laying down their armed demand.
% FOUNDING_PROBLEM_CORROBORATION: The crown's own repeated reissues (1216, 1217, 1225) and the 1297 confirmation extracted under threat of withheld taxation attest the problem's persistence from an adverse-interest seat; monastic chroniclers outside the compact (Matthew Paris; the Dunstable annalist) document continuing royal evasions; the routine application of the clause by royal justices in the plea rolls attests the procedural need independently of baronial advantage. No attestation comes from the unfree majority — they were never asked.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__feudal_prerogative_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__feudal_prerogative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__feudal_prerogative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_clause_39__feudal_prerogative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__feudal_prerogative_reading, 0.31, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).
:- end_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.31 — modest by this reading's own design: the clause burdens traditional authority only at the margin (the crown keeps wardship, fiscal power, and jurisdiction over the unfree), and most of the measured extraction sits in the entrenchment effect — procedural security distributed as a class privilege, with the settlement's peace financed by the majority's exclusion. Suppression (0.30 at interval end) is a raw structural property, deliberately unscaled by power or scope: it measures the coercive holding-power the clause itself required, which was high in 1215 (an armed committee of twenty-five with distraining power over royal castles) and decayed toward normalized judicial administration by 1350 — the suppression_requirement series models that decay, including the temporary re-tightening around the 1297 confirmation crisis. Theater (0.30) rises across the interval as confirmations, cathedral readings, and oath ceremonies accumulate around a clause the courts still genuinely apply; the performative share grows without the function atrophying inside this window. Accessibility_collapse (0.35) is low because alternatives persist throughout — the crown repeatedly sought repudiation, rival jurisdictions (Church courts, manorial courts, the writ system) operated alongside, and nothing here forecloses other arrangements the way a natural limit does. Resistance (0.55) is the crown's own record: immediate repudiation in 1215, papal annulment, grudging minority-era reissues, the 1297 crisis. All three temporal series share one grid (1215-1350 at seven points) so no metric is sampled against another's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently because the same settlement is three different things from three positions. From the crown's seat the clause is a bounded concession it administers itself — a price paid once in 1215 for a stability that preserved nearly everything else it held. From the baronial seat it is a won and repeatedly renewed guarantee, the shield over persons, heirs, and land. From the villein seat the same document is the settlement made over their heads: the protection they watch distributed by rank is a cost they bear with no voice in any reissue. The chronicler's analytical seat sees all three at once, which is why the contemporary record reads as both reverence and grievance.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (baronage_and_knights, lesser_free_tenants) drive low directionality for the protected class — the guarantee subsidizes them, and their constrained exit (land, lineage, tenure) does not change that they sit at the subsidized end. The victim declarations split the paying side. The crown bears the formal burden (payer role, constrained exit — it cannot exit its own legal order), which the structural derivation would read as near-full target; the override to 0.65 corrects for the crown's dual position as the compact's chief beneficiary — the settlement preserved the dynasty in 1215 and left the bulk of prerogative intact, so the crown is substantially subsidized by the very limit it pays. The unfree peasantry derive high directionality from the payer declaration plus trapped exit: their payment is exclusion itself, held in place by the settlement's stability. No override is needed for the beneficiary class — the derivation already seats them at the subsidized end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — elite insecurity against arbitrary royal action — stayed live across the whole interval, so no mandatrophy is declared and none is due: the clause's function did not outlive its mandate inside this window. The classification work the type claim does is boundary-keeping in both directions: a pure-coordination label would erase the villein exclusion's asymmetric cost (the settlement's peace is financed by the majority's rightlessness), while a pure-extraction label would erase the genuine crown-baron collective-action solution and the crown's own compact-level gain. The theater rise is ceremonialization around a live function, not atrophy — judicial application is real at 1350 — so a degraded-inertial reading would be premature inside the interval; the drift_state records where the atrophy pressure actually arrives, later, and from this reading's own success.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the feudal_prerogative_reading of the magna_carta_clause_39 kernel — clause 39 as a rank-bound procedural privilege stabilizing the hierarchical order. What structural changes would a sibling reading produce if adopted, and where exactly is the disagreement located?',
    'Compare against the sibling stories'' authored victim sets and epsilon: the liberal_due_process_reading universalizes the protected class (all persons against arbitrary state power); the originalist_limitation_reading narrows the guarantee to the specific royal abuses documented in the 1215 context. The disagreement is located in two structural elements: the referent of ''free man'' (legal class term vs. universal person) and the source of the guarantee''s legitimacy (enacted class settlement vs. principle latent in the text vs. documented-abuse remedy).',
    'Under the liberal sibling the victim set expands beyond the crown''s bounded prerogative and the excluded majority to every person denied procedure, and epsilon rises; under the originalist sibling epsilon falls toward a narrow abuse-remedy. This file''s classification is unaffected — the siblings are separate constraints with their own stable epsilon per the epsilon-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a contested kernel; sibling deltas and the location of the dispute.').

omega_variable(
    villein_exclusion_status,
    'Is the unfree majority''s exclusion from the guarantee an extraction through this settlement — the price at which elite peace was purchased and the hierarchical order preserved — or merely the boundary of its scope, a fact about who the clause addresses rather than a cost it imposes?',
    'Counterfactual settlement analysis: whether a 1215 universal-procedure clause would have altered manorial obligations or lord-court jurisdiction; manorial court records read against the charter''s operative scope across the interval.',
    'If the exclusion is constitutive extraction, epsilon sits near the authored 0.31 and the tangled_rope claim holds; if it is mere scope, epsilon drops toward 0.15 and the arrangement trends toward pure coordination between crown and elites with the majority simply outside it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(villein_exclusion_status, conceptual, 'Whether this reading''s defining narrowness is extraction or boundary.').

omega_variable(
    enforcement_normalization,
    'After the coercive phase of 1215-1217, did the clause''s persistence depend on active enforcement at all, or did it hold by normalization and interest-convergence between crown and elites?',
    'Institutional history: distraint records after 1217, the 1297 confirmation crisis, judicial enforcement dockets, and the frequency of baronial re-extraction episodes (1258, 1264, 1297).',
    'If enforcement became dispensable after normalization, the active-enforcement leg of the hybrid claim is historically contingent — strongest early, weakest late; the suppression_requirement series models exactly this decay with the temporary re-tightening around 1297.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_normalization, empirical, 'Whether the constraint''s enforcement requirement decayed into normalization across the interval.').

omega_variable(
    law_of_land_content,
    'What did ''the law of the land'' concretely require between 1215 and 1350 — crown-administered custom, established feudal procedure, or an independent standard the crown could not set alone?',
    'Judicial practice analysis: plea rolls, writ-system procedure, and the 1354 statutory gloss rendering the formula as ''due process of law''.',
    'A crown-defined content makes the guarantee partly theatrical (theater_ratio understated at the margin); an independent content makes it a genuine check on the crown (epsilon''s crown-facing component understated).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(law_of_land_content, empirical, 'Operative content of the clause''s central formula across the interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__feudal_prerogative_reading, 1215, 1350).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1215, 0.08).
narrative_ontology:measurement(magn_tr_t1225, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1225, 0.1).
narrative_ontology:measurement(magn_tr_t1250, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1250, 0.14).
narrative_ontology:measurement(magn_tr_t1275, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1275, 0.18).
narrative_ontology:measurement(magn_tr_t1300, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1300, 0.22).
narrative_ontology:measurement(magn_tr_t1325, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1325, 0.26).
narrative_ontology:measurement(magn_tr_t1350, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1350, 0.3).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1215, 0.2).
narrative_ontology:measurement(magn_be_t1225, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1225, 0.24).
narrative_ontology:measurement(magn_be_t1250, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1250, 0.26).
narrative_ontology:measurement(magn_be_t1275, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1275, 0.28).
narrative_ontology:measurement(magn_be_t1300, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1300, 0.29).
narrative_ontology:measurement(magn_be_t1325, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1325, 0.3).
narrative_ontology:measurement(magn_be_t1350, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1350, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1215, 0.6).
narrative_ontology:measurement(magn_su_t1225, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1225, 0.48).
narrative_ontology:measurement(magn_su_t1250, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1250, 0.4).
narrative_ontology:measurement(magn_su_t1275, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1275, 0.36).
narrative_ontology:measurement(magn_su_t1300, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1300, 0.38).
narrative_ontology:measurement(magn_su_t1325, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1325, 0.31).
narrative_ontology:measurement(magn_su_t1350, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1350, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__feudal_prerogative_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__originalist_limitation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Magna Carta, clause 39' decomposes into three structurally distinct constraints — this feudal reading (protection as rank-bound privilege; epsilon ~0.31), the liberal due-process reading (universal individual guarantee; higher epsilon, expanded victim set), and the originalist limitation reading (remedy for documented 1215 abuses only; lower epsilon). The readings share a fixed text but assign different protected classes and different legitimacy sources, so each carries its own stable epsilon per the epsilon-invariance principle. The feudal reading is upstream: its centuries of operation created the text-prestige and the 'law of the land' formula that the liberal reading later reinterprets, so edges run from this file to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_clause_39__feudal_prerogative_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
