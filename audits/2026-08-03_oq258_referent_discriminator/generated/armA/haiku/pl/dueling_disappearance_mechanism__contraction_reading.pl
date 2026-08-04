% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__contraction_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: dueling_disappearance_mechanism__contraction_reading
 *   human_readable: Dignity-Culture Displacement of Honor-Culture Axioms (Contraction Reading)
 *   domain: cultural/legal/historical
 *
 * SUMMARY:
 *   This is the CONTRACTION READING of dueling's disappearance: the
 *   constraint that made dueling a central institution in honor-culture
 *   societies (18th–19th century American South and gentry everywhere)
 *   vanished not because legal prohibition worked, but because the cultural
 *   substrate itself—the axioms recognizing honor-as-status,
 *   reputation-as-property, and fighting-as-remedial—became illegible to the
 *   rising generation. Dignity culture, grounded in individual rights and
 *   bodily autonomy, was not a deliberate project imposed from above but an
 *   emergent substrate that displaced honor-culture axioms. Once dignity
 *   culture became the water, honor-culture practitioners were drowning in
 *   it—their framework could not be maintained because the people around them
 *   no longer recognized its categories. This is distinct from the
 *   institutional-displacement reading (dueling lost to courts, arbitration,
 *   libel law) and the overdetermined-composite reading (prohibition +
 *   modernization + trauma all contributed). This reading makes the claim
 *   that dignity-culture displacement was the sufficient cause of dueling's
 *   cognitive disappearance, not merely one factor among others.
 *
 * KEY AGENTS:
 *   - Honor-culture practitioners (planter/merchant gentry): powerful, identity-locked exit, face illegibility of their axioms
 *   - Dignity-culture practitioners (educated professional class): powerful, mobile, benefit from new substrate
 *   - Women as protected category: powerless under honor-culture (could not duel), gain individual standing under dignity culture
 *   - State authority: institutional agenda-setter, bans dueling by statute post-Civil War, but prohibition only works because substrate has shifted
 *   - Religious institutions: observer seat, opposition to dueling was constant, but only became culturally resonant when dignity-culture axioms took hold
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.62).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.41).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dignity-Culture Displacement of Honor-Culture Axioms (Contraction Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "cultural/legal/historical").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, '064cfd5b-0dae-4448-9ca7-745ab50f2a96').
narrative_ontology:cs_kernel_codification('064cfd5b-0dae-4448-9ca7-745ab50f2a96', distributed).
narrative_ontology:cs_authority_grounding('064cfd5b-0dae-4448-9ca7-745ab50f2a96', distributed).
narrative_ontology:cs_reading_relation('064cfd5b-0dae-4448-9ca7-745ab50f2a96', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('064cfd5b-0dae-4448-9ca7-745ab50f2a96', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('064cfd5b-0dae-4448-9ca7-745ab50f2a96', foundational, dignity_culture_substrate_inevitability).
narrative_ontology:cs_axiom_status(dignity_culture_substrate_inevitability, holdable).
narrative_ontology:cs_axiom_grounding('064cfd5b-0dae-4448-9ca7-745ab50f2a96', dignity_culture_substrate_inevitability, deontological).
narrative_ontology:cs_axiom('064cfd5b-0dae-4448-9ca7-745ab50f2a96', secondary, honor_culture_axiom_illegibility).
narrative_ontology:cs_axiom_status(honor_culture_axiom_illegibility, holdable).
narrative_ontology:cs_axiom_grounding('064cfd5b-0dae-4448-9ca7-745ab50f2a96', honor_culture_axiom_illegibility, empirically_contingent).
narrative_ontology:cs_reference_frame('064cfd5b-0dae-4448-9ca7-745ab50f2a96', honor_culture_framework).
narrative_ontology:cs_drift_state('064cfd5b-0dae-4448-9ca7-745ab50f2a96', dignity_culture_substrate_fully_established, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('064cfd5b-0dae-4448-9ca7-745ab50f2a96', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, dignity_culture_practitioners).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, women_as_protected_category).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, emerging_professional_classes).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gentlemen of the planter and merchant classes whose social standing, masculine identity, and reputation-management framework depended on dueling's availability as a redress mechanism. Once dignity culture became the substrate, their honor-frame became illegible to the rising generation—not suppressed by law alone, but displaced by a cultural substrate that no longer recognized their axioms. Exit would require abandoning the framework constituting their identity.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    powerful, biographical, identity_locked, national).

% The emerging educated, urban, professional middle class and Northern establishment whose self-concept rested on individual rights, bodily autonomy, rational dispute-resolution, and personal dignity rather than honor reputation. Dueling's disappearance was not a cost they bore but a cultural inevitability—their framework became the water dueling practitioners were drowning in.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dignity_culture_practitioners, beneficiary,
    powerful, generational, mobile, national).

% Under honor-culture axioms, women could be insulted but not properly avenged by duel (they could not duel); their honor was defended by male relatives' willingness to fight. Dignity culture recognized women as rights-bearing individuals whose bodily autonomy and personal standing could not be defended by proxy violence. They had no structural seat in the dueling system and gained standing as the system contracted.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, women_as_protected_category, beneficiary,
    powerless, generational, constrained, national).

% Lawyers, physicians, engineers, merchants dependent on reputation but operating in institutional contexts (bar associations, medical societies, commercial courts) where dueling was disruptive to their practice. Dignity culture's institutions (libel law, professional discipline, commercial dispute resolution) provided alternative reputation-management and dispute-resolution mechanisms that did not require lethal combat.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, emerging_professional_classes, beneficiary,
    moderate, generational, mobile, national).

% After the Civil War and Reconstruction, state monopoly on legitimate violence hardened; states banned dueling by statute and prosecuted it. But the reading does not rest on legal prohibition—prohibition was effective because the cultural substrate had already shifted. Without the dignity-culture displacement, prohibition would have remained dead-lettered as it had been in earlier periods.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, state_authority, agenda_setter,
    institutional, generational, analytical, national).

% Churches and religious authorities (especially Northern Protestant establishments) had condemned dueling for centuries on Christian grounds; the shift to dignity-culture axioms made their theological arguments finally culturally legible. Their opposition was constant; what changed was the substrate that made opposition resonant.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, religious_institutions, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__contraction_reading, dignity_culture_practitioners).
narrative_ontology:fixing_cost_class(dueling_disappearance_mechanism__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dueling under honor-culture axioms provided a mechanism for reputation recovery and masculine status maintenance within a framework where personal honor was constitutive of social standing. It was a coordination problem: if you were insulted and did not fight, your reputation contracted; others were coordinated on the expectation that fighting restored standing. Dignity culture did not solve this problem—it dissolved it by rejecting the axioms (honor-as-status, reputation-as-property, fighting-as-remedial) that made it a problem at all.
% TRANSFER_FUNCTION: Dueling as a constraint transferred risk of death and disability from the insulter to the insulted and to their families. The honor-culture reading construed this as a fair exchange: the insulter risked death to recover reputation; the insulted risked death to defend it. Dignity culture inverts the moral sign: the constraint transferred lethal risk in the course of defending something (honor) that dignity culture does not recognize as a legitimate good worth dying for.
% ABSENT_VOICES: Enslaved people and Indigenous peoples were structurally outside the honor-culture framework (they could not duel to defend honor and their honor was not recognized as defensible); they had no seat in the system. Their exclusion persisted under dignity culture, but for different reasons—under dignity culture the framework became about rights-bearing individuals, and the structural exclusion of these groups from rights-bearing status became visible and contestable in ways it had not been under honor-culture axioms.
% DISAPPEARANCE_RATIONALE: If dignity culture had not displaced honor culture—if the substrate had remained honor-based—dueling would have persisted despite legal prohibition. The constraint's persistence depends not on enforcement machinery alone but on the cultural illegibility of its axioms. Dueling disappeared because practitioners died out and their successors were born into a world where honor-as-status and reputation-via-combat were not just illegal but cognitively foreign. If the substrate had not shifted, dueling would have persisted as a covert or ritualized practice, as it did when prohibition was attempted without cultural displacement.
% FOUNDING_PROBLEM: The founding problem of honor-culture dueling: how does a gentleman whose reputation has been damaged by insult recover his standing in a society where honor is constitutive of social position and reputation is defended through demonstrated willingness to risk death? Dueling solved this by providing a lethal mechanism that proved courage and upheld honor-claims.
% FOUNDING_PROBLEM_CORROBORATION: Historical consensus from scholars outside the honor-culture tradition (Wyatt-Brown, Greenberg, Anbinder, Parmenter) documents that dueling declined precipitously after 1860 despite decades of legal prohibition beforehand. The precipitous character of the decline (not gradual erosion but generational discontinuity) and its correlation with the rise of dignity-culture axioms in published writing, schooling, and institutional norms (rather than with enforcement intensity) supports the contraction reading over a pure institutional-displacement or legal-prohibition hypothesis. Religious and intellectual opposition to dueling was voiced consistently from the 17th century onward; what changed at mid-19th century was the cultural substrate that made opposition resonant to the rising generation.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dueling_disappearance_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as MOUNTAIN because dignity-culture axioms (individual rights, bodily autonomy, dignity-rather-than-honor) are presented as irreversible cultural substrates once established. This reading treats dignity culture not as a chosen value system but as a near-inevitable emergence from print culture, mass education, and market societies—it becomes part of the water in which subsequent generations swim. Extractiveness is high (0.62–0.72 mid-interval) because the constraint extracts cost from honor-culture practitioners whose framework becomes illegible; they cannot maintain the dueling system because nobody around them recognizes its axioms anymore. Accessibility collapse is very high (0.89) because once dignity-culture axioms become the substrate, the alternative (honor-culture framework) collapses entirely—you cannot resurrect it without changing the deeper water. Resistance is substantial (0.74) because honor-culture practitioners resisted the displacement fiercely through the 19th century; they did not go quietly. Theater ratio is low (0.18 at end) because once the substrate has fully shifted, dueling is not theater—it is unthinkable, not performed performatively. The measurement series show the rising peak around 1870 (post-Civil War, when state prohibition hardened AND dignity culture was solidifying) and then the collapse as the constraint disappears because its cognitive referents have vanished. The gap between the claim (mountain) and the metrics (moderate-high extractiveness, substantial resistance) reflects the core ambiguity: is this an irreversible natural substrate, or a constructed constraint that beneficiaries (dignity-culture practitioners, women, professional classes) experienced as inevitable but could have been resisted if honor-culture practitioners had possessed sufficient power or coordination? This gap is the measurement the reading exists to take.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (honor-culture practitioners) and the beneficiary seats (dignity-culture practitioners) should compute dramatically different type readings from the same constraint. From the honor-culture perspective, this looks like extraction and destruction—their framework is being erased. From the dignity-culture perspective, this looks like cultural inevitability and natural-law emergence—dignity values are simply the right way humans think, not a constructed constraint. The engine should compute the honor-culture seat as experiencing this as snare-adjacent (victims, extraction, identity-locked exit, high suppression once the substrate fully turns against them). The dignity-culture seat should compute as experiencing this as mountain (naturally emerging, beneficial to them, with low accessibility-collapse for alternatives they reject anyway). This divergence is the point: the same constraint looks like forced cultural extinction from one seat and inevitable human progress from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor-culture practitioners experience this constraint as extraction: they are the victims, bearing the cost of illegibility and cultural extinction. Their d-value is high (~0.85–0.95) because they are trapped—their exit is identity-locked and the cultural substrate itself has shifted so that continuing to honor-duel is not a live option but a cognitive impossibility for their children. Dignity-culture practitioners experience the constraint as natural law: they are the beneficiaries, experiencing dignity culture as simply how the world works, not as a constraint imposed on others. Their d-value is low (~0.05–0.15). Women and professional classes are beneficiaries gaining standing and exit options they did not have under honor-culture axioms. State authority sits in the analytical seat (d~0.5): they observe and eventually enforce the prohibition, but they do not author the displacement—they are responding to a substrate shift that has already occurred.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of honor-culture dueling (how does a gentleman recover status when insulted?) is DEAD in dignity-culture societies. But the constraint persists as cognitive illegibility—modern readers do not understand why dueling was ever rational, and that illegibility is the constraint's operation. This is not a piton (which persists despite dead founding problem through theatrical maintenance and inertia); it is a vanished constraint whose only residue is historical incomprehension. Once the substrate fully shifted, dueling did not persist in any form—not even ceremonially, not even in covert practice. The reading implies that dignified-constraint axioms became so thoroughly embedded that honor-culture axioms are not just suppressed but cognitively unavailable. This is different from suppression by force (which dignity culture does not employ on dueling—by the time prohibition is enforced, nobody wants to duel anyway). The mandatrophy here is substrate-level: the problem the constraint solved became unthinkable because the entire framework for recognizing it as a problem vanished.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substrate_shift_vs_constructed_displacement,
    'Is dignity culture an irreversible substrate that emerged inevitably from deeper structural conditions (print, markets, education), or a constructed ideological displacement that beneficiaries (professional classes, women, Northern establishment) deliberately imposed on honor-culture practitioners?',
    'Historical reconstruction of actual causal mechanisms: trace the publication, institutional adoption, and educational dissemination of dignity-culture axioms across the 19th century; examine whether this was an elite project or a broader emergence. Evidence would differentiate between substrate-inevitability and deliberate-displacement framings.',
    'If dignity culture emerged inevitably from structural conditions, the constraint is plausibly a mountain (natural substrate displacement). If it was deliberate ideological displacement, it is better classified as snare (extraction via cultural suppression), and beneficiary victims include honor-culture practitioners whose framework was destroyed. The reading''s mountain classification depends on treating dignity culture as substratial inevitability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substrate_shift_vs_constructed_displacement, conceptual, 'Whether dignity-culture displacement is an inevitable substrate emergence or a constructed ideological project.').

omega_variable(
    identity_lock_mechanism_honor_culture,
    'How complete was the identity-lock that bound honor-culture practitioners to the honor framework? Could a practitioner have exited to dignity-culture axioms by deliberately changing their values, or was the framework so constitutive of self-concept that exit was psychologically unavailable?',
    'Historical analysis of individual and family narratives of honor-culture practitioners during the transition (diaries, letters, testimony); study the trajectories of those who did attempt to transition and what costs they bore. Did any practitioners successfully ''convert'' to dignity-culture values, and what psychological and social work did that require?',
    'If the identity-lock was complete (no exit available even in principle), then the constraint extracts substantially and victims are clearly identified. If some practitioners did exit successfully, then the exit_options are more granular—some identity-locked, some constrained but not locked. This would lower d-value for some practitioners and refine the victim classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_honor_culture, empirical, 'Whether honor-culture identity-lock was psychologically/socially complete or partially penetrable.').

omega_variable(
    dignity_culture_axiom_contestability,
    'Within dignity-culture societies, are the axioms (individual rights, bodily autonomy, rational dispute-resolution) genuinely embraced as natural and inevitable, or are they sustained through ongoing suppression of honor-culture alternatives?',
    'Examine the legal, social, and institutional effort required to sustain dignity-culture axioms. If the axioms are natural and inevitable, they should require minimal enforcement. If they are sustained against persistent challenge, that suggests they are more constructed than inevitable.',
    'If dignity culture requires substantial suppression to maintain, it is less clearly a mountain and more clearly a constructed constraint. This would lower the accessibility_collapse score and raise the resistance score. The reading''s claim that dignity culture is an inevitable substrate would be weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_culture_axiom_contestability, empirical, 'Whether dignity-culture axioms are sustained naturally or through active enforcement.').

omega_variable(
    counterfactual_honor_culture_persistence,
    'If dignity culture had not emerged (counterfactually), could legal prohibition alone have eliminated dueling, or would dueling have persisted indefinitely despite prohibition?',
    'Comparative historical analysis: examine societies that experienced legal prohibition without dignity-culture displacement (if any exist); study historical cases where prohibition was attempted without substrate shift and assess persistence. Examine covert or ritualized dueling in 20th-century societies that retained stronger honor-culture axioms (Mediterranean, Latin American, certain military cultures).',
    'If dueling persists in non-dignity-culture societies, this supports the contraction reading''s causal claim: dignity-culture displacement is necessary and sufficient. If dueling disappeared due to prohibition alone in some contexts, the institutional-displacement reading is strengthened and dignity-culture displacement becomes one factor among several, not the pivot cause.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_honor_culture_persistence, empirical, 'Whether dignity-culture displacement was the sufficient cause of dueling''s disappearance, or whether prohibition alone could have achieved it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 1770, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1770, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1770, 0.08).
narrative_ontology:measurement_basis(duel_tr_t1770, observed).
narrative_ontology:measurement(duel_tr_t1810, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1810, 0.1).
narrative_ontology:measurement_basis(duel_tr_t1810, observed).
narrative_ontology:measurement(duel_tr_t1850, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1850, 0.12).
narrative_ontology:measurement_basis(duel_tr_t1850, observed).
narrative_ontology:measurement(duel_tr_t1870, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1870, 0.22).
narrative_ontology:measurement_basis(duel_tr_t1870, observed).
narrative_ontology:measurement(duel_tr_t1890, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1890, 0.28).
narrative_ontology:measurement_basis(duel_tr_t1890, observed).
narrative_ontology:measurement(duel_tr_t1920, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1920, 0.18).
narrative_ontology:measurement_basis(duel_tr_t1920, observed).

% Extraction over time
narrative_ontology:measurement(duel_be_t1770, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1770, 0.58).
narrative_ontology:measurement_basis(duel_be_t1770, observed).
narrative_ontology:measurement(duel_be_t1810, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1810, 0.6).
narrative_ontology:measurement_basis(duel_be_t1810, observed).
narrative_ontology:measurement(duel_be_t1850, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1850, 0.65).
narrative_ontology:measurement_basis(duel_be_t1850, observed).
narrative_ontology:measurement(duel_be_t1870, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1870, 0.72).
narrative_ontology:measurement_basis(duel_be_t1870, observed).
narrative_ontology:measurement(duel_be_t1890, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1890, 0.68).
narrative_ontology:measurement_basis(duel_be_t1890, observed).
narrative_ontology:measurement(duel_be_t1920, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1920, 0.62).
narrative_ontology:measurement_basis(duel_be_t1920, observed).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1770, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1770, 0.22).
narrative_ontology:measurement_basis(duel_su_t1770, observed).
narrative_ontology:measurement(duel_su_t1810, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1810, 0.28).
narrative_ontology:measurement_basis(duel_su_t1810, observed).
narrative_ontology:measurement(duel_su_t1850, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1850, 0.35).
narrative_ontology:measurement_basis(duel_su_t1850, observed).
narrative_ontology:measurement(duel_su_t1870, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1870, 0.52).
narrative_ontology:measurement_basis(duel_su_t1870, observed).
narrative_ontology:measurement(duel_su_t1890, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1890, 0.48).
narrative_ontology:measurement_basis(duel_su_t1890, observed).
narrative_ontology:measurement(duel_su_t1920, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1920, 0.41).
narrative_ontology:measurement_basis(duel_su_t1920, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dueling_disappearance_mechanism__contraction_reading, 0.12).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__institutional_displacement_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% The dueling_disappearance_mechanism kernel decomposes into three constraint stories, each instantiating the same historical phenomenon from a different structural reading. The contraction_reading emphasizes dignity-culture substrate displacement; the institutional_displacement_reading emphasizes functional replacement via courts and commercial law; the overdetermined_composite_reading denies any single sufficient cause and argues for multiple independent factors. These are not perspectives on one constraint—they are three different constraints with different ε values, different victim sets, different type classifications at different seats. They are linked via network.affects_constraints to signal that they compete in the same interpretive space (the same kernel) and that evidence resolving one reading affects the others' plausibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dueling_disappearance_mechanism__contraction_reading, powerless, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
