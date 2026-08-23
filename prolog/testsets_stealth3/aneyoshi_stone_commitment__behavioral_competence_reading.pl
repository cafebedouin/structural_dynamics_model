% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__behavioral_competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: aneyoshi_stone_commitment__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Directive as Live Land-Use Rule (Behavioral-Competence Reading)
 *   domain: disaster anthropology / commitment systems / temporal institutional analysis
 *
 * SUMMARY:
 *   Stone markers stand on the terrace above the hamlet of Aneyoshi on the
 *   Sanriku coast of Iwate Prefecture, erected after the 1896 Meiji Sanriku
 *   tsunami and re-inscribed after the 1933 Showa Sanriku tsunami at the
 *   documented run-up line, carved with directives to descendants not to
 *   build dwellings below them. Across the 78 years from the 1933
 *   re-inscription to the 2011 Tohoku tsunami, the hamlet rebuilt uphill of
 *   the stones and kept every dwelling above the line without any enforcement
 *   machinery, legal backing, or institutional continuity beyond the hamlet's
 *   own observance. On 11 March 2011 the tsunami ran up to just below the
 *   village: every resident of Aneyoshi survived, while neighboring low-lying
 *   settlements on the same coast suffered fatalities and destroyed housing.
 *   This story authors the standing arrangement the story is about - the
 *   hamlet's stone-anchored settlement regime - assessed on its own
 *   operation: a directive that continuously constrained building-location
 *   decisions for 78 years at near-zero extraction. The geophysical fact the
 *   stones encode (run-up recurrence on this coast) is a separate natural-law
 *   constraint; this story is the social arrangement that encodes and applies
 *   it.
 *
 * KEY AGENTS:
 *   - stone_erector_survivors: founding agenda-setter (moderate/trapped) - 1896 and 1933 tsunami survivors who inscribed the directive at the run-up line as addressed testimony to descendants
 *   - aneyoshi_hamlet_association: administering agenda-setter (organized/constrained) - maintains the stones, leads the memorial rites, governs siting by consensus; administers a line it also lives under
 *   - aneyoshi_village_residents: net beneficiary bearing the diffuse setback cost (organized/constrained) - the seat that pays the forborne seaward strip and collected the 2011 survival margin
 *   - neighboring_shoreline_hamlets: excluded comparison class (moderate/constrained) - same coast, contrary siting practice, no seat in the hamlet's deliberations
 *   - iwate_prefecture_authorities: observer (institutional/analytical) - post-2011 run-up documentation and reconstruction siting guidance
 *   - post2011_coastal_planners: incidental beneficiary (institutional/mobile) - inherit a validated template for community-held setback norms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, 0.15).
domain_priors:theater_ratio(aneyoshi_stone_commitment__behavioral_competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__behavioral_competence_reading, rope).
narrative_ontology:human_readable(aneyoshi_stone_commitment__behavioral_competence_reading, "Aneyoshi Stone Directive as Live Land-Use Rule (Behavioral-Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__behavioral_competence_reading, "disaster anthropology / commitment systems / temporal institutional analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__behavioral_competence_reading, '03c3863c-11b6-40d4-bdff-615cb9ac792a').
narrative_ontology:cs_kernel_codification('03c3863c-11b6-40d4-bdff-615cb9ac792a', fixed_text).
narrative_ontology:cs_authority_grounding('03c3863c-11b6-40d4-bdff-615cb9ac792a', lineage).
narrative_ontology:cs_interpretation_layer_present('03c3863c-11b6-40d4-bdff-615cb9ac792a').
narrative_ontology:cs_reading_relation('03c3863c-11b6-40d4-bdff-615cb9ac792a', aneyoshi_stone_commitment__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('03c3863c-11b6-40d4-bdff-615cb9ac792a', foundational, survivor_inscription_carries_binding_force).
narrative_ontology:cs_axiom_status(survivor_inscription_carries_binding_force, holdable).
narrative_ontology:cs_axiom_grounding('03c3863c-11b6-40d4-bdff-615cb9ac792a', survivor_inscription_carries_binding_force, conventional).
narrative_ontology:cs_axiom('03c3863c-11b6-40d4-bdff-615cb9ac792a', secondary, compliance_vindicated_by_2011_runup).
narrative_ontology:cs_axiom_status(compliance_vindicated_by_2011_runup, holdable).
narrative_ontology:cs_axiom_grounding('03c3863c-11b6-40d4-bdff-615cb9ac792a', compliance_vindicated_by_2011_runup, empirically_contingent).
narrative_ontology:cs_reference_frame('03c3863c-11b6-40d4-bdff-615cb9ac792a', survivor_issued_land_use_regulation).
narrative_ontology:cs_drift_state('03c3863c-11b6-40d4-bdff-615cb9ac792a', post_2011_reassessment, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('03c3863c-11b6-40d4-bdff-615cb9ac792a', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_village_residents).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, post2011_coastal_planners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_hamlet_association).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_village_residents).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, intergenerational_inundation_memory).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, precautionary_setback_principle).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, vernacular_risk_regulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hamlet survivors of the 1896 Meiji Sanriku tsunami, which killed roughly 22,000 people on the Sanriku coast, and of the 1933 Showa Sanriku tsunami, erected and re-inscribed stone markers at the documented run-up line with directives addressed to descendants not to build dwellings below them. They acted from direct experience of what seaward rebuilding costs; their authority was testimony, not office. By the 1960s the last of them had died, leaving the stones and the annual memorial observance as their continuing instruction.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, stone_erector_survivors, agenda_setter,
    moderate, generational, trapped, local).

% The hamlet's district association maintains the stones, leads the annual memorial rites at them, and communicates the building line to each new household. It decides, informally and by consensus, where new construction may stand; the seaward strip below the stones has remained garden and field throughout. Its members are villagers themselves, so the same households that administer the line live under it.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_hamlet_association, agenda_setter,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_hamlet_association, beneficiary).

% Roughly thirty households fishing and farming on a small coastal terrace in Iwate. They live uphill of the stones and walk farther to their boats than a shore-side hamlet would; in exchange, on 11 March 2011 the tsunami ran up to just below the village and every resident survived. Leaving the hamlet is possible but costly - houses, family graves, and fishing grounds are all here - so the practical alternative to heeding the line is building below it, which no household has done since 1933.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_village_residents, beneficiary,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_village_residents, payer).

% Settlements along the same Sanriku coast that built at or near sea level - some behind seawalls, some relying on memory and warning practice alone. They were never party to Aneyoshi's communal undertaking; their 2011 losses in nearby low-lying districts stand as the outcome the stones' directive encoded, but they had no seat in the hamlet's deliberations, and their contrary building practice was visible to Aneyoshi for decades without being adopted.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, neighboring_shoreline_hamlets, excluded,
    moderate, biographical, constrained, regional).

% Prefectural government and its disaster-reconstruction offices. Before 2011 they took no role in the hamlet's building line; afterward they documented the survival, surveyed the run-up, and cited the case in reconstruction siting guidance. They observe and record; they neither administer the stones nor bear the setback.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, iwate_prefecture_authorities, observer,
    institutional, generational, analytical, regional).

% Regional and national planners, engineers, and researchers who after 2011 treat the Aneyoshi stones as a validated template for community-held setback norms and for transmitting hazard memory across generations. They gain a working precedent; they bear none of its costs and were absent from its making.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, post2011_coastal_planners, beneficiary,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_village_residents).
narrative_ontology:fixing_cost_class(aneyoshi_stone_commitment__behavioral_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the settlement-placement problem under recurrent tsunami hazard on a coast whose run-up elevations outlast human memory: a durable, self-interpreting high-water datum - the stone at the 1896/1933 run-up line with a carved directive - coordinates where households may build, requiring no maps, no legal system, and no institutional continuity beyond the hamlet's own observance.
% TRANSFER_FUNCTION: Transfers almost nothing: no money, labor, or status moves between seats. What moves is risk exposure - every household forgoes the seaward building strip (a diffuse convenience and land-use cost borne by the residents themselves) in exchange for standing above the documented run-up line. The arrangement converts a catastrophic tail risk into a small annual inconvenience, paid and collected by the same seat.
% ABSENT_VOICES: No seat inside the hamlet objects - the building line has been consensus since 1933. Absent: the neighboring shoreline hamlets that built at sea level and bore the 2011 fatalities; they are the counterfactual the stones encode, but they were never in Aneyoshi's deliberations and their contrary practice, visible for decades, changed nothing. Also absent: pre-2011 regional planners, who classed vernacular markers as folklore rather than regulation and so never engaged the arrangement that was holding the line.
% DISAPPEARANCE_RATIONALE: The hamlet's building line sits where the stones sit. Had the directive's force lapsed in, say, the postwar building boom, the flat seaward strip - convenient to boats, attractive in calm decades - would have been built on, following exactly the pattern that cost neighboring settlements their dead in 2011. The 2011 run-up reached the old shoreline just below the village; the survival margin is the arrangement operating, not an accident of topography alone.
% FOUNDING_PROBLEM: After the 1896 Meiji Sanriku tsunami killed roughly 22,000 people on this coast, and again after 1933, survivors faced a transmission problem: human memory of inundation fades in two to three generations, no map or law then held the run-up line, and the natural rebuild impulse points seaward. The stones encode the run-up elevation as a permanent local instruction - do not build your homes below this point - addressed to descendants who would otherwise have no way to know it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the 2011 Tohoku run-up surveys place the inundation line just below the hamlet's building line and above the old shoreline; tsunami sedimentology independently records the 1896 and 1933 sand sheets the stones memorialize; and Iwate Prefecture's post-2011 reconstruction guidance cites the case. None of these sources benefits from the stones' directive.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 0.05, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is near-floor (0.05 at interval end) because the arrangement transfers nothing: the only cost is the residents' own forgone seaward building strip, paid and collected by the same seat, while the 2011 run-up demonstrated the payoff. Suppression (0.15) reflects ordinary social-norm pressure in a thirty-household hamlet - no enforcement machinery, legal backing, or sanction was ever built, and the temporal series therefore omits suppression_requirement entirely: the enforcement picture is static by construction and the scalar captures it. Theater (0.15 at interval end) traces an honest arc: as living memory of 1896 and 1933 faded, the stones' ceremonial share grew (0.10 in 1933 to 0.35 by 2005) while the building line held throughout; the 2011 run-up, which reached the old shoreline just below the village, collapsed the ceremonial share back down (0.15) and re-activated the directive function - an event-driven re-validation at 1960, 1968, and 2011, not an oscillating cycle. Accessibility collapse is moderate (0.45): the alternative - building below the stones - remained physically and legally open for the entire interval and was foreclosed by consensus and transmitted testimony, not by the arrangement's structure. Resistance (0.10) is minimal; no organized seat ever formed against the line. The claimed type and the metrics are authored independently from the same structural read; nothing here is tuned toward a predicted engine output.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From inside the resident seat, the stones are a standing instruction from the dead that has never required enforcement - the setback reads as inheritance, and the seat sits near the beneficiary end despite paying the cost, because the tail payoff dwarfs the annual inconvenience. From the hamlet association's seat, administering the line and obeying it are the same act. From the prefecture's and the planners' seats, the arrangement is evidence and template - engaged analytically, borne by no one. From the neighboring hamlets' seat - the excluded comparison class at the same nominal power level - the same coastline offered no such line: their differentiated experience is not power but history and exit, since they built seaward because their memory institutions failed, not because they were weaker. Same-level actors diverge on constraint-specific factors: proximity to the 1933 founders' testimony and the survival of the marker itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared (aneyoshi_village_residents, post2011_coastal_planners) and no victims exist, so every seated agent derives a low directionality and effective extraction is damped toward subsidy for all of them. The resident seat is kept near-symmetric rather than fully subsidized by its secondary payer position - it bears the arrangement's entire cost - but the net position is strongly beneficiary because the cost is a forgone option while the benefit is survival. The planners' seat is the purest beneficiary: template value with mobile exit and zero cost borne. No directionality overrides are needed; the structural derivation from the beneficiary declarations and exit options captures every seat. Receipt surface: the arrangement's entire surplus - the 2011 survival margin - accrues to the resident seat itself; no seat captures a transfer, because the only cost is the residents' own forgone option. gain_flow therefore names that seat as the accrual point; it is mutual accrual, not capture. Fixing (abandoning the line) is prohibitive relative to its benefit: the 2011 run-up line sits just below the village, so removal converts a survival margin into fatality exposure for the seat that would do the removing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - intergenerational transmission of inundation memory on a coast whose recurrence intervals exceed institutional memory spans - is live, re-confirmed by the 2011 run-up; the arrangement has never outlived its function, so no mandatrophy is declared and the R5 mismatch consumer reads status=live against verdict=world_rearranges with no flag. The classification guards against the opposite misread as well: with no extracted surplus, no victim seat, and no enforcement machinery, reading the arrangement as pure extraction would require inventing a capturer that does not exist. The mid-interval theater rise is not decay in progress: it reversed on contact with the run-up, which is what a live directive does and what a husk cannot. Identity dynamics reinforce rather than constitute the arrangement - heeding the stones is part of hamlet self-understanding, but residents are not identity-locked (leaving is possible and some do); if the identity frame broke, compliance would rest on the demonstrated payoff, which 2011 supplied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contest_behavioral_vs_husk,
    'Which reading instantiates the aneyoshi_stone_commitment kernel: this behavioral-competence reading (the directive retained operational force in building-location decisions 1933-2011) or the commemorative-husk sibling (the directive decayed to symbolic observance without behavioral constraint)?',
    'Hamlet construction and land-use records for 1933-2011 (was any dwelling ever sited seaward of the stones, or even proposed?), plus ethnographic evidence of the directive''s role in siting decisions. The disagreement between the readings is located in the single structural element of behavioral constraint on siting.',
    'If the sibling reading is adopted, this referent is re-authored with near-zero extraction but high theatrical maintenance (memorial upkeep without regulatory function), the 2011 survival must be re-attributed to topography or chance, and the classification moves from low-extraction coordination toward inertial memorial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_behavioral_vs_husk, empirical, 'Kernel-reading contest: live land-use rule versus commemorative husk.').

omega_variable(
    runup_survival_causal_link,
    'Is the 2011 survival causally attributable to compliance with the directive, or would the 2011 run-up have stopped below the hamlet terrace regardless of where dwellings stood?',
    'Counterfactual inundation modeling siting dwellings at the old shoreline versus the actual building line, benchmarked against neighboring hamlets at comparable terrace elevation that built seaward and suffered fatalities in 2011.',
    'If topography alone explains survival, the vindicated-proposition set shrinks and the directive''s coordination value drops from life-saving to precautionary; the low-extraction coordination classification likely survives either way, but this reading''s causal axiom weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(runup_survival_causal_link, empirical, 'Whether the 2011 survival margin is the directive''s causal product.').

omega_variable(
    norm_self_enforcement_ambiguity,
    'Is the 78-year compliance maintained by transmitted conviction alone (a self-sustaining communal norm), or by latent social-sanction capacity that never needed to fire?',
    'Ethnographic elicitation in the hamlet: what reception would a household proposing to build below the stones meet today, and any historical instances of seaward building proposals and their outcome.',
    'If latent sanction capacity exists, the authored suppression understates the arrangement''s coercive surface and the minimal-overhead coordination reading needs qualification toward enforced coordination; if conviction alone suffices, suppression is genuinely near-floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_self_enforcement_ambiguity, empirical, 'Self-sustaining norm versus latent enforcement behind the compliance record.').

omega_variable(
    vernacular_regulation_generalizability,
    'Does the Aneyoshi arrangement instantiate a reproducible class of survivor-inscribed hazard regulation, or an idiosyncratic product of one hamlet''s catastrophe history and small size?',
    'Comparative corpus work on the Sanriku coast''s many inscribed tsunami markers: which carried comparable behavioral force and what distinguishes them (hamlet size, run-up visibility, memorial practice); post-2011 adoption attempts elsewhere.',
    'If idiosyncratic, the network edge to post-2011 reconstruction siting policy overstates structural influence and this story should be read as a singleton rather than a class exemplar; if reproducible, the edge marks a real transmission channel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vernacular_regulation_generalizability, conceptual, 'Class exemplar versus idiosyncratic artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_behavioral_tr_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1933, 0.1).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t1945, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1945, 0.12).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t1960, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t1968, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1968, 0.18).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t1983, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1983, 0.25).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t1993, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1993, 0.3).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t2005, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 2011, 0.15).

% Extraction over time
narrative_ontology:measurement(aneyoshi_behavioral_be_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1933, 0.12).
narrative_ontology:measurement(aneyoshi_behavioral_be_t1945, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(aneyoshi_behavioral_be_t1960, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1960, 0.09).
narrative_ontology:measurement(aneyoshi_behavioral_be_t1968, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1968, 0.08).
narrative_ontology:measurement(aneyoshi_behavioral_be_t1983, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1983, 0.07).
narrative_ontology:measurement(aneyoshi_behavioral_be_t1993, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1993, 0.06).
narrative_ontology:measurement(aneyoshi_behavioral_be_t2005, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 2005, 0.06).
narrative_ontology:measurement(aneyoshi_behavioral_be_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 2011, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_commitment__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__behavioral_competence_reading, information_standard).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment__commemorative_husk_reading).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__behavioral_competence_reading, sanriku_tsunami_runup_envelope).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__behavioral_competence_reading, post2011_sanriku_reconstruction_setback).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Aneyoshi stone commitment' covers three structurally distinct claims, decomposed per epsilon-invariance. Upstream: sanriku_tsunami_runup_envelope - the geophysical fact that tsunami run-up on this coast reaches and exceeds the stones' elevation; a natural-law constraint with negligible extraction, which this arrangement encodes but does not constitute. Sibling reading: aneyoshi_stone_commitment__commemorative_husk_reading - the same stones read as memorial artifact with the directive decayed to symbolic observance; that story authors near-zero extraction with high theatrical maintenance and is this reading's direct contradictory on the single element of behavioral constraint. Downstream: post2011_sanriku_reconstruction_setback - the reconstruction-era siting policy that cites the Aneyoshi survival. This story authors only the behavioral-competence reading, with its own stable epsilon (0.05), beneficiary structure, and metric series.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
