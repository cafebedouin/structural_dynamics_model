% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Dignity Culture as Irreversible Cognitive Substrate (Dueling Disappearance — Contraction Reading)
 *   domain: cultural_anthropology/legal_history/cognitive_systems
 *
 * SUMMARY:
 *   This reading claims that dueling disappeared not through legal
 *   prohibition, institutional replacement, or economic competition, but
 *   through the irreversible cognitive displacement of honor-culture axioms
 *   by dignity-culture frameworks. In honor culture, personal standing could
 *   be attacked and damaged by insult; dueling provided the only credible
 *   mechanism for restoration. In dignity culture, personal worth is
 *   intrinsic and cannot be damaged by external accusation; therefore,
 *   dueling becomes unintelligible as a status mechanism. This reading treats
 *   dignity culture as a cognitive substrate—a framework that, once
 *   established in a population, makes the logic of honor-based combat
 *   unthinkable. It is offered as ONE READING of a contested kernel
 *   (dueling_disappearance_mechanism); sibling readings
 *   (institutional_displacement_reading, overdetermined_composite_reading)
 *   offer alternative mechanisms. This reading is a mountain constraint:
 *   dignity culture is not constructed by any agent; it emerges as an
 *   irreversible epistemic substrate whose establishment is largely beyond
 *   individual choice or institutional design.
 *
 * KEY AGENTS:
 *   - honor_culture_practitioners: Identity-locked parties whose framework made dueling intelligible; experience the transition as cognitive erasure
 *   - dignity_culture_practitioners: Organized beneficiaries of the new framework; do not actively extract but inhabit an incompatible epistemic space
 *   - rising_educated_cohort: External to honor-culture sensemaking; learn dignity axioms without cognitive dissonance
 *   - legal_institutional_apparatus: Observer; statutes were inert until the cultural substrate shifted
 *   - women_and_economic_dependents: Powerless excluded beneficiaries; removed from violence without agency in the transition
 *   - aristocratic_military_class: Identity-locked payers; experience acute cognitive bind as military honor connects to dueling logic
 *   - analytical_observer: External analysis of the mechanism itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.21).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.08).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.21).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dignity Culture as Irreversible Cognitive Substrate (Dueling Disappearance — Contraction Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "cultural_anthropology/legal_history/cognitive_systems").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, 'c276ba6e-4397-4a58-a444-7b3c9d6347e4').
narrative_ontology:cs_kernel_codification('c276ba6e-4397-4a58-a444-7b3c9d6347e4', distributed).
narrative_ontology:cs_authority_grounding('c276ba6e-4397-4a58-a444-7b3c9d6347e4', diffuse_epistemic).
narrative_ontology:cs_reading_relation('c276ba6e-4397-4a58-a444-7b3c9d6347e4', dueling_disappearance_mechanism__institutional_displacement_reading, forecloses).
narrative_ontology:cs_reading_relation('c276ba6e-4397-4a58-a444-7b3c9d6347e4', dueling_disappearance_mechanism__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('c276ba6e-4397-4a58-a444-7b3c9d6347e4', foundational, dignity_axiom_cognitive_irreversibility).
narrative_ontology:cs_axiom_status(dignity_axiom_cognitive_irreversibility, holdable).
narrative_ontology:cs_axiom_grounding('c276ba6e-4397-4a58-a444-7b3c9d6347e4', dignity_axiom_cognitive_irreversibility, deontological).
narrative_ontology:cs_axiom('c276ba6e-4397-4a58-a444-7b3c9d6347e4', foundational, honor_axiom_incompatibility_with_dignity).
narrative_ontology:cs_axiom_status(honor_axiom_incompatibility_with_dignity, holdable).
narrative_ontology:cs_axiom_grounding('c276ba6e-4397-4a58-a444-7b3c9d6347e4', honor_axiom_incompatibility_with_dignity, deontological).
narrative_ontology:cs_reference_frame('c276ba6e-4397-4a58-a444-7b3c9d6347e4', honor_culture_epistemic_hegemony).
narrative_ontology:cs_drift_state('c276ba6e-4397-4a58-a444-7b3c9d6347e4', contemporary_post_dignity_culture_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('c276ba6e-4397-4a58-a444-7b3c9d6347e4', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, dignity_culture_practitioners).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, non_violent_conflict_resolution_episteme).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, rising_educated_cohort).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, women_and_economic_dependents).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, aristocratic_military_class).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__contraction_reading, cultural_axioms_constrain_intelligibility).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__contraction_reading, dignity_framework_irreversibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operated within a framework where personal honor was the primary currency of identity and social standing. Dueling enforced this axiom: refusal to duel meant social death (loss of reputation, marriage prospects, military commission, civic standing). When dignity culture became dominant, the honor framework became unintelligible to the rising generation—practitioners found their entire worldview and status-negotiation mechanism delegitimized. They could not exit the framework by choice; it was erased around them, rendering their identity investments worthless.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    powerful, biographical, identity_locked, regional).

% Adopted a framework where personal dignity—intrinsic worth not dependent on external validation through combat—became the organizing principle. This framework was incompatible with honor-culture's logic (one could not simultaneously claim intrinsic dignity and accept that refusal to fight meant loss of standing). The shift was not chosen by individuals but emerged as a cultural-cognitive substrate that made honor logic increasingly unintelligible. Dignity practitioners benefit from this shift only insofar as they avoid the trap of the honor system entirely; they do not actively extract from honor practitioners—rather, the substrate beneath them vanishes.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dignity_culture_practitioners, beneficiary,
    organized, generational, arbitrage, regional).

% Educated in frameworks (Enlightenment philosophy, Christian ethics, bourgeois respectability norms) that positioned dueling as irrational and dishonorable. For this cohort, the dignity axiom was learned, not inherited; they could adopt it without cognitive dissonance because they were not born into honor-culture's sensemaking apparatus. Their mobility came from being positioned outside the honor framework from the start.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, rising_educated_cohort, beneficiary,
    organized, biographical, mobile, regional).

% Enforced anti-dueling statutes (present in most Western jurisdictions by the late 1700s), but these statutes were inert until the cultural substrate shifted. The apparatus recorded and narrated the cultural change; it did not cause it. Once dignity culture became the epistemic ground, enforcement became unnecessary—dueling was already unthinkable, not merely illegal.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, legal_institutional_apparatus, observer,
    institutional, generational, analytical, regional).

% Were excluded from the honor system as active participants (women could not fight; economic dependents' honor was mediated through male relatives) but bore the costs of dueling violence (death of male providers, trauma, social instability). The shift to dignity culture removed one mechanism of violence from their environment, though they had no voice in the cultural transition itself and remained powerless in the new system.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, women_and_economic_dependents, beneficiary,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__contraction_reading, women_and_economic_dependents, excluded).

% Military honor was explicitly connected to willingness to die in combat for the state. Dueling was a sublimation of this norm into civilian life. As dignity culture displaced honor culture, the military class faced a cognitive bind: they could not maintain the honor axiom while accepting the dignity frame. Some adapted (reframing military service as duty rather than honor); many experienced the transition as an assault on their foundational identity.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, aristocratic_military_class, payer,
    powerful, biographical, identity_locked, national).

% Stands outside the cultural transition, observing the mechanism by which one epistemic framework became unintelligible without active suppression of the other. This reading asserts that dueling disappeared not because it was prohibited or economically outcompeted, but because the cognitive substrate that made it intelligible as a status mechanism was irreversibly replaced by an incompatible framework.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(dueling_disappearance_mechanism__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Honor culture coordinated status negotiation and conflict resolution through a shared understanding that personal standing could be damaged by accusation and only restored by demonstrated willingness to risk death in combat. Dueling enforced the credibility of status claims within this framework. The dignity framework coordinated by asserting intrinsic worth independent of external validation, making combat-based status repair unintelligible.
% TRANSFER_FUNCTION: The constraint transfers cognitive stability and identity coherence from honor-culture practitioners to dignity-culture practitioners and their descendants. Honor practitioners lose the primary mechanism by which their identity-investments were legible; dignity practitioners gain a system that does not require violence to maintain status. Women and economic dependents gain removal of a source of death and instability, though they are excluded from the cultural negotiation itself.
% ABSENT_VOICES: Honor-culture practitioners who did not live to see the transition. Dead practitioners (those killed in duels, or who took their own lives rather than accept the new framework) have no voice in whether the transition was legitimate. Living practitioners at the moment of transition experienced the change as something imposed on them, not chosen. Their objections were systematically unintelligible to the rising dignity-culture generation, creating a form of epistemic exclusion.
% DISAPPEARANCE_RATIONALE: If dignity culture had not displaced honor culture as the epistemic substrate, dueling would likely have persisted despite legal prohibition—prohibition statutes were routinely violated in the 1700s, and legal enforcement alone cannot eliminate a practice whose participants see it as constitutive of identity. The disappearance of dueling required the disappearance of the framework that made dueling intelligible as a legitimate status mechanism.
% FOUNDING_PROBLEM: Honor culture faced a coordination problem: how to establish credible status claims in contexts where reputation damage could be fatal to social and economic position? Dueling solved this by creating a costly signal (willingness to risk death) that could not be faked. The practice enforced honor-culture's core axiom: that personal standing could be attacked and must be defended through demonstration of courage.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship (Frevert, Wyatt-Brown, Simpson) confirms that honor-culture's founding problem—coordinating credible status claims—was real and dueling was an effective solution within that framework. Dignity-culture scholarship (Taylor, Appiah) confirms that once dignity became the epistemic substrate, the honor-culture problem became unintelligible: one's intrinsic worth could not be damaged by insult, so dueling became nonsensical. The corroboration comes from historians and anthropologists OUTSIDE the practicing communities—neither honor nor dignity practitioners authored this diagnosis while the framework transition was occurring.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.21, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__contraction_reading_tests).

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
 *   Extractiveness is very low (0.21) because this reading does not posit an actor extracting value from others—rather, it describes a cognitive shift that renders one framework unintelligible. Suppression is minimal (0.08) for the same reason: dignity culture did not suppress honor culture through force; honor practitioners lost the framework itself. Theater ratio is low (0.12) because the constraint's operation is not performative—it is the genuine cognitive substrate. Accessibility collapse is very high (0.91) because once dignity culture became established, honor-culture logic was genuinely inaccessible—practitioners could not revive it by choosing to; the epistemic ground had shifted beneath them. Resistance is extremely low (0.04) because there was no organized resistance to a cognitive substrate shift; practitioners of honor culture experienced the change as inevitable, not as something that could be resisted through collective action. The coercion_grid shows stakes_inflation (how much honor-culture standing mattered) declining from 0.90+ to near-zero by 1900, while accessibility_collapse (how completely alternatives closed off) remained high throughout—the mechanism is the disappearance of stakes, not the closure of alternatives. Suppression requirement rises modestly (0.02 to 0.08) as legal enforcement increased, but remains low relative to the cognitive collapse.
 *
 * PERSPECTIVAL GAP:
 *   This reading treats the constraint as a mountain from the analytical observer's seat: dignity culture is irreversible, emergent, and not constructed by any single agent. But from the honor practitioner's seat, it operates with massive extractive and suppressive force (the extraction of meaning, the suppression of a viable framework). This gap is not a measurement error; it is the central observation of the contraction reading: a cognitive substrate shift that is genuinely irreversible (mountain-like) but massively asymmetric in its effects (snare-like for those who lose the framework).
 *
 * DIRECTIONALITY LOGIC:
 *   Honor practitioners' directionality is high (d near 0.8–0.9) because identity-lock gives them trapped exit options; they cannot leave honor culture without losing their identity. Dignity practitioners' directionality is low (d near 0.1–0.2) because they inherit the framework without coercion. Women and dependents, though beneficiaries, have zero agency in the transition (excluded role), so their directionality remains near zero—they benefit passively, not through any extraction they impose.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of honor culture—to coordinate status claims through credible willingness to risk death—is definitively dead by 1900. The founding problem (how to establish credible status in honor-culture contexts) is obsolete because honor culture itself became unintelligible. However, the constraint persists at a ceremonial level: dueling echoes in formal apologies, trial combat in legal procedures, and honor systems in military and aristocratic contexts. The theater_ratio measurement (0.12 by 1900) captures this: the remaining enforcement activity is largely theatrical—statutes are enforced, but there are few duels to prosecute because the behavior has become unthinkable. This is a mandatrophy candidate: a constraint whose founding mandate is dead but whose institutional shells persist as theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_mechanism_clarity,
    'What is the specific cognitive mechanism by which honor-culture practitioners became unable to revive or transmit their framework? Was it active disavowal by the rising generation, loss of social contexts where honor-culture sensemaking could be learned and practiced, or genuine cognitive incompatibility between honor and dignity axioms?',
    'Historical analysis of family correspondence, educational curricula, and institutional settings where honor-culture transmission was attempted post-1750. Ethnographic analogy from contemporary communities undergoing framework transition (e.g., honor-culture persistence in some Mediterranean and Middle Eastern societies despite legal prohibition). Cognitive science of concept acquisition under axiom mismatch.',
    'If the mechanism is active disavowal, the constraint is better modeled as snare (suppression of the old framework). If it is context-loss, it is better modeled as piton (inertial decay). If it is genuine cognitive incompatibility, the mountain classification holds—dignity axioms are truly irreversible once adopted at population scale.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_clarity, empirical, 'Whether honor-culture loss was active suppression, contextual decay, or cognitive incompatibility.').

omega_variable(
    dignity_culture_emergence_locus,
    'Did dignity culture emerge as a genuinely new epistemic substrate, or did it represent a rediscovery/reframing of pre-existing philosophical and religious traditions (Stoicism, Christianity, Enlightenment ethics)? If the latter, what made these traditions suddenly hegemonic by 1800 when they had existed for centuries?',
    'Intellectual history analysis of when dignity-axiom texts (Kant, Rousseau, Smith) achieved institutional embedding (universities, church reformulations, legal codes). Sociological analysis of what institutional and economic changes made dignity frameworks advantageous (commercialization, literacy expansion, nation-state formation). Counterfactual: would dignity axioms have become hegemonic without the Industrial Revolution and literacy expansion?',
    'If dignity culture is genuinely new, it supports the mountain reading (emergence of an unprecedented epistemic substrate). If it is rediscovery, the reading must account for WHY pre-existing traditions suddenly dominated—which may implicate institutional mechanisms not captured by the pure cognitive-substrate model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_culture_emergence_locus, conceptual, 'Whether dignity culture was a novel emergence or a rediscovery that became hegemonic under new conditions.').

omega_variable(
    honor_culture_residue_and_reversal,
    'Honor-culture practices persist in measurable form in 21st-century military, aristocratic, and some regional/ethnic communities. Can these residues be revived to full cultural hegemony, or are they genuinely foreclosed by dignity-culture substrate establishment?',
    'Analysis of honor-culture revival movements (neo-honor ideologies in some political contexts; reactive honor-framing in response to perceived dignity-culture marginalization). Cognitive-frame switching experiments: can individuals fluent in dignity frameworks temporarily adopt honor-logic reasoning, and if so, how stable is the adoption? Scenario analysis: what conditions would need to obtain for honor culture to re-establish epistemic hegemony.',
    'If honor culture can be revived to hegemony under certain conditions, the constraint is not truly mountain-like (reversible, contingent on political/institutional conditions). If revival is cognitively impossible once dignity becomes established, the mountain classification is strengthened. If revival is theoretically possible but requires catastrophic institutional collapse, the constraint is mountain-like for all practical timescales.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honor_culture_residue_and_reversal, empirical, 'Whether dignity-culture establishment is truly irreversible or contingent on institutional maintenance.').

omega_variable(
    framing_alternative_institutional_mechanism,
    'This reading posits a pure cognitive-substrate mechanism. An alternative framing (the institutional_displacement_reading sibling) posits that dueling fell to institutional substitution: courts, banking, libel law solved the same problems dueling solved more efficiently. Could both mechanisms be true simultaneously (cognitive shift AND institutional competition)?',
    'Timeline analysis: which came first, the decline in dueling practices or the institutional alternatives? Were there periods of simultaneous decline in dueling AND rise in institutional substitutes? Cross-cultural analysis: in regions where institutional alternatives emerged WITHOUT dignity-culture shift (e.g., some societies that adopted modern courts while maintaining honor-culture frameworks), did dueling persist longer?',
    'If institutional alternatives were necessary but not sufficient (dueling persisted despite available alternatives until dignity culture shifted), the contraction reading holds primacy. If institutional alternatives were sufficient (dueling declined in societies with modern courts even without dignity-culture shift), the institutional_displacement_reading has equal or primary claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_alternative_institutional_mechanism, empirical, 'Whether the cognitive-substrate mechanism is primary or co-equal with institutional displacement.').

omega_variable(
    committer_kernel_contest,
    'This constraint instantiates ONE reading of a contested kernel (dueling_disappearance_mechanism). This reading claims the contraction mechanism (dignity-culture substrate shift) is the primary cause of dueling''s disappearance. What is the structural irreducibility of this reading versus its siblings? Can the readings be nested (one causing the others) or are they genuinely parallel causal claims?',
    'Formal logical analysis of the causal graphs each reading posits: does the contraction reading''s cause (dignity-culture emergence) necessarily precede or subsume the causes posited by institutional_displacement and overdetermined_composite readings? Empirical sequencing: timeline of dignity-axiom establishment (1650–1800?) versus institutional alternatives (1700–1850?) versus legal prohibition (1750–1900?). Philosophical analysis: are dignity and honor axioms genuinely incommensurable (cannot coexist in one framework) or merely competitive?',
    'If the readings are genuinely parallel (any one sufficient, but only one operative in a given historical trajectory), each remains a valid constraint story. If one reading''s cause subsumes the others (e.g., dignity-culture establishment necessarily produces institutional alternatives as downstream effects), the readings form a causal hierarchy and the contraction reading has primary explanatory status. If the readings are incommensurable in the logical sense (dignity axiom FORECLOSES honor axiom in any coherent framework), the contraction and institutional readings cannot both be true.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_kernel_contest, conceptual, 'Whether the contraction, institutional, and composite readings are parallel, nested, or incompatible causal mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 1600, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1600, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1600, 0.08).
narrative_ontology:measurement_basis(duel_tr_t1600, observed).
narrative_ontology:measurement(duel_tr_t1700, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement_basis(duel_tr_t1700, observed).
narrative_ontology:measurement(duel_tr_t1750, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1750, 0.12).
narrative_ontology:measurement_basis(duel_tr_t1750, observed).
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1800, 0.14).
narrative_ontology:measurement_basis(duel_tr_t1800, observed).
narrative_ontology:measurement(duel_tr_t1850, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1850, 0.13).
narrative_ontology:measurement_basis(duel_tr_t1850, observed).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement_basis(duel_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(duel_be_t1600, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1600, 0.18).
narrative_ontology:measurement_basis(duel_be_t1600, observed).
narrative_ontology:measurement(duel_be_t1700, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1700, 0.19).
narrative_ontology:measurement_basis(duel_be_t1700, observed).
narrative_ontology:measurement(duel_be_t1750, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1750, 0.2).
narrative_ontology:measurement_basis(duel_be_t1750, observed).
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1800, 0.21).
narrative_ontology:measurement_basis(duel_be_t1800, observed).
narrative_ontology:measurement(duel_be_t1850, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1850, 0.22).
narrative_ontology:measurement_basis(duel_be_t1850, observed).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1900, 0.21).
narrative_ontology:measurement_basis(duel_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1600, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1600, 0.02).
narrative_ontology:measurement_basis(duel_su_t1600, observed).
narrative_ontology:measurement(duel_su_t1700, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1700, 0.03).
narrative_ontology:measurement_basis(duel_su_t1700, observed).
narrative_ontology:measurement(duel_su_t1750, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1750, 0.05).
narrative_ontology:measurement_basis(duel_su_t1750, observed).
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1800, 0.08).
narrative_ontology:measurement_basis(duel_su_t1800, observed).
narrative_ontology:measurement(duel_su_t1850, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1850, 0.07).
narrative_ontology:measurement_basis(duel_su_t1850, observed).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1900, 0.08).
narrative_ontology:measurement_basis(duel_su_t1900, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1600, tn=1900
narrative_ontology:measurement(duel_grid_01, dueling_disappearance_mechanism__contraction_reading, accessibility_collapse(class), 1600, 0.87).
narrative_ontology:measurement(duel_grid_02, dueling_disappearance_mechanism__contraction_reading, accessibility_collapse(class), 1900, 0.94).
narrative_ontology:measurement(duel_grid_03, dueling_disappearance_mechanism__contraction_reading, accessibility_collapse(individual), 1600, 0.88).
narrative_ontology:measurement(duel_grid_04, dueling_disappearance_mechanism__contraction_reading, accessibility_collapse(individual), 1900, 0.95).
narrative_ontology:measurement(duel_grid_05, dueling_disappearance_mechanism__contraction_reading, accessibility_collapse(organizational), 1600, 0.85).
narrative_ontology:measurement(duel_grid_06, dueling_disappearance_mechanism__contraction_reading, accessibility_collapse(organizational), 1900, 0.93).
narrative_ontology:measurement(duel_grid_07, dueling_disappearance_mechanism__contraction_reading, accessibility_collapse(structural), 1600, 0.89).
narrative_ontology:measurement(duel_grid_08, dueling_disappearance_mechanism__contraction_reading, accessibility_collapse(structural), 1900, 0.95).
narrative_ontology:measurement(duel_grid_09, dueling_disappearance_mechanism__contraction_reading, resistance(class), 1600, 0.03).
narrative_ontology:measurement(duel_grid_10, dueling_disappearance_mechanism__contraction_reading, resistance(class), 1900, 0.04).
narrative_ontology:measurement(duel_grid_11, dueling_disappearance_mechanism__contraction_reading, resistance(individual), 1600, 0.01).
narrative_ontology:measurement(duel_grid_12, dueling_disappearance_mechanism__contraction_reading, resistance(individual), 1900, 0.02).
narrative_ontology:measurement(duel_grid_13, dueling_disappearance_mechanism__contraction_reading, resistance(organizational), 1600, 0.02).
narrative_ontology:measurement(duel_grid_14, dueling_disappearance_mechanism__contraction_reading, resistance(organizational), 1900, 0.05).
narrative_ontology:measurement(duel_grid_15, dueling_disappearance_mechanism__contraction_reading, resistance(structural), 1600, 0.02).
narrative_ontology:measurement(duel_grid_16, dueling_disappearance_mechanism__contraction_reading, resistance(structural), 1900, 0.03).
narrative_ontology:measurement(duel_grid_17, dueling_disappearance_mechanism__contraction_reading, stakes_inflation(class), 1600, 0.9).
narrative_ontology:measurement(duel_grid_18, dueling_disappearance_mechanism__contraction_reading, stakes_inflation(class), 1900, 0.04).
narrative_ontology:measurement(duel_grid_19, dueling_disappearance_mechanism__contraction_reading, stakes_inflation(individual), 1600, 0.92).
narrative_ontology:measurement(duel_grid_20, dueling_disappearance_mechanism__contraction_reading, stakes_inflation(individual), 1900, 0.05).
narrative_ontology:measurement(duel_grid_21, dueling_disappearance_mechanism__contraction_reading, stakes_inflation(organizational), 1600, 0.88).
narrative_ontology:measurement(duel_grid_22, dueling_disappearance_mechanism__contraction_reading, stakes_inflation(organizational), 1900, 0.03).
narrative_ontology:measurement(duel_grid_23, dueling_disappearance_mechanism__contraction_reading, stakes_inflation(structural), 1600, 0.91).
narrative_ontology:measurement(duel_grid_24, dueling_disappearance_mechanism__contraction_reading, stakes_inflation(structural), 1900, 0.02).
narrative_ontology:measurement(duel_grid_25, dueling_disappearance_mechanism__contraction_reading, suppression(class), 1600, 0.02).
narrative_ontology:measurement(duel_grid_26, dueling_disappearance_mechanism__contraction_reading, suppression(class), 1900, 0.09).
narrative_ontology:measurement(duel_grid_27, dueling_disappearance_mechanism__contraction_reading, suppression(individual), 1600, 0.01).
narrative_ontology:measurement(duel_grid_28, dueling_disappearance_mechanism__contraction_reading, suppression(individual), 1900, 0.06).
narrative_ontology:measurement(duel_grid_29, dueling_disappearance_mechanism__contraction_reading, suppression(organizational), 1600, 0.02).
narrative_ontology:measurement(duel_grid_30, dueling_disappearance_mechanism__contraction_reading, suppression(organizational), 1900, 0.08).
narrative_ontology:measurement(duel_grid_31, dueling_disappearance_mechanism__contraction_reading, suppression(structural), 1600, 0.03).
narrative_ontology:measurement(duel_grid_32, dueling_disappearance_mechanism__contraction_reading, suppression(structural), 1900, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dueling_disappearance_mechanism__contraction_reading, 0.12).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__institutional_displacement_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% The dueling_disappearance_mechanism kernel has three constraint stories representing three distinct causal mechanisms for dueling's cultural disappearance: contraction_reading (dignity-culture substrate displacement), institutional_displacement_reading (institutional competition), and overdetermined_composite_reading (multiple independent sufficient causes). Each story is ε-invariant and characterizes a structurally distinct constraint. The contraction reading asserts that institutional competition and legal prohibition were inert until the cognitive substrate shifted; this creates a causal precedence relationship captured by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dueling_disappearance_mechanism__contraction_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
