% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__competence_transmission_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__competence_transmission_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: catastrophe_memory_survival__competence_transmission_reading
 *   human_readable: Catastrophe-Memory Ritual as Practical Survival Competence Transmission
 *   domain: religious studies/collective memory/ritual practice
 *
 * SUMMARY:
 *   This constraint reads catastrophe-memory ritual as a transmission
 *   mechanism for practical survival competence: precise timing rules,
 *   resource-management protocols, and family-mobilization sequences
 *   originally calibrated to recurring subsistence crises, encoded in a
 *   performance structure durable enough to cross generations who never lived
 *   through the originating catastrophe. The reading takes the practical
 *   payload as the thing ritual is FOR — symbolic and identity functions are
 *   incidental scaffolding around a functional knowledge archive. As
 *   displaced communities carry the ritual into new environments, the
 *   embedded content resurfaces as genuinely useful adaptive knowledge
 *   (diaspora communities are the clear beneficiary here). But in settled
 *   descendant communities where the original crisis conditions have receded,
 *   the same ritual cycle persists as a full-cost performance obligation
 *   whose practical content has gone quietly inert, extracting labor and
 *   compliance from younger performers while the interpretive authority over
 *   'what it means to do it right' concentrates with elder knowledge-holders.
 *   This is a moderate, rising extraction profile, not a severe one: the
 *   coordination function (competence transmission across generational gaps)
 *   is real and historically load-bearing, which is what keeps this a
 *   tangled_rope rather than a snare.
 *
 * KEY AGENTS:
 *   - ritual_elders_and_knowledge_holders: administer content and timing, benefit from interpretive authority
 *   - diaspora_communities_retaining_adaptive_capacity: primary functional beneficiary, ritual content maps onto real adaptive need
 *   - settled_descendant_communities_losing_practical_content: bear the cost of a full ceremonial apparatus whose practical payload has gone silent for their conditions
 *   - younger_generation_ritual_performers: identity-locked performers bearing labor cost without access to underlying rationale
 *   - outside_ethnographers_and_survival_researchers: analytical observers testing whether the practical function actually survives decoupled from crisis recurrence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, 0.52).
domain_priors:suppression_score(catastrophe_memory_survival__competence_transmission_reading, 0.44).
domain_priors:theater_ratio(catastrophe_memory_survival__competence_transmission_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__competence_transmission_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__competence_transmission_reading, "Catastrophe-Memory Ritual as Practical Survival Competence Transmission").
narrative_ontology:topic_domain(catastrophe_memory_survival__competence_transmission_reading, "religious studies/collective memory/ritual practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__competence_transmission_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__competence_transmission_reading, '3ed749da-7f40-4457-9be7-a0ce79705428').
narrative_ontology:cs_kernel_codification('3ed749da-7f40-4457-9be7-a0ce79705428', implicit).
narrative_ontology:cs_authority_grounding('3ed749da-7f40-4457-9be7-a0ce79705428', practice).
narrative_ontology:cs_interpretation_layer_present('3ed749da-7f40-4457-9be7-a0ce79705428').
narrative_ontology:cs_reading_relation('3ed749da-7f40-4457-9be7-a0ce79705428', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('3ed749da-7f40-4457-9be7-a0ce79705428', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('3ed749da-7f40-4457-9be7-a0ce79705428', foundational, ritual_content_is_primarily_operational_knowledge).
narrative_ontology:cs_axiom_status(ritual_content_is_primarily_operational_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('3ed749da-7f40-4457-9be7-a0ce79705428', ritual_content_is_primarily_operational_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('3ed749da-7f40-4457-9be7-a0ce79705428', secondary, symbolic_effects_are_incidental_to_transmission_function).
narrative_ontology:cs_axiom_status(symbolic_effects_are_incidental_to_transmission_function, holdable).
narrative_ontology:cs_axiom_grounding('3ed749da-7f40-4457-9be7-a0ce79705428', symbolic_effects_are_incidental_to_transmission_function, instrumental).
narrative_ontology:cs_reference_frame('3ed749da-7f40-4457-9be7-a0ce79705428', operational_knowledge_archive_function).
narrative_ontology:cs_drift_state('3ed749da-7f40-4457-9be7-a0ce79705428', post_settlement_stabilization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3ed749da-7f40-4457-9be7-a0ce79705428', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities_retaining_adaptive_capacity).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, ritual_elders_and_knowledge_holders).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, settled_descendant_communities_losing_practical_content).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, younger_generation_ritual_performers).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__competence_transmission_reading, ritual_as_functional_knowledge_archive).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the timing, resource-management, and family-protocol content embedded in the ritual cycle — deciding what gets taught, to whom, and in what sequence. They hold interpretive authority over which practical details are 'authentic' transmission versus later accretion, and derive social standing and material support (teaching fees, deference, resource-allocation priority) from being the ones who still know how to read the calendar correctly.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, ritual_elders_and_knowledge_holders, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__competence_transmission_reading, ritual_elders_and_knowledge_holders, beneficiary).

% Carry the ritual cycle into new territories after displacement and find that its embedded timing and resource rules — storage schedules, migration windows, water-management sequences — still map usefully onto unfamiliar environments once adapted. They benefit from having inherited an operational toolkit disguised as religious observance, and can modify the practical layer without abandoning the form.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities_retaining_adaptive_capacity, beneficiary,
    moderate, generational, mobile, continental).

% Continue performing the full ritual calendar in a settled, resource-stable environment where the original survival function (famine timing, drought-cycle storage, emergency kinship mobilization) no longer applies to daily life. They pay in labor, time, and resources to maintain a full ceremonial apparatus whose practical payload has gone silent, while being told the form itself is what protects them — a bet they cannot easily test since the underlying crisis it prepares for rarely recurs.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, settled_descendant_communities_losing_practical_content, payer,
    powerless, biographical, trapped, local).

% Are required to memorize and perform the full ritual sequence as a condition of family and community standing, without being taught the underlying practical rationale (why this timing, why this resource allocation) that would let them adapt it. Their exit is blocked less by external barrier than by identity fusion — refusing the ritual reads as refusing kinship itself, even when they privately doubt its practical content still applies.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, younger_generation_ritual_performers, payer,
    powerless, immediate, identity_locked, local).

% Study whether the ritual's embedded content still functions as transmittable survival knowledge or has become inert form. They compare diaspora adaptations against settled-community performances to test whether the practical payload survives decoupled from crisis recurrence, and publish findings that neither elders nor performers directly control.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, outside_ethnographers_and_survival_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__competence_transmission_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__competence_transmission_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual cycle originally coordinated dispersed households around shared timing for resource storage, migration, and mutual aid during recurring subsistence crises — a distributed memory system that outlasted any single generation's direct experience of catastrophe.
% TRANSFER_FUNCTION: Moves interpretive authority and social standing to those who administer the ritual's timing and protocol content, and moves labor, compliance time, and unquestioned deference from performers (especially the young) to that administering layer — with practical adaptive benefit flowing disproportionately to communities still facing conditions the embedded knowledge addresses.
% ABSENT_VOICES: Younger performers who privately suspect the ritual's practical content no longer applies rarely have standing to say so without appearing to reject kinship or tradition; their doubt is structurally excluded from the interpretive conversation that decides what the ritual 'means.'
% DISAPPEARANCE_RATIONALE: Diaspora communities would lose a real adaptive toolkit and would need to reconstruct resource-timing knowledge from scratch under novel conditions — a genuine loss. Settled descendant communities, by contrast, might notice little functional change beyond the social reorganization of losing a status hierarchy built around ritual expertise, since the practical content they're maintaining is largely already inert for their situation. The two reading-relevant populations would experience disappearance very differently.
% FOUNDING_PROBLEM: Recurring subsistence catastrophe (famine cycles, drought, forced migration) required a way to transmit precise operational knowledge — when to store, when to move, whom to call on — across generations who had not personally lived through the crisis, using a memorization and performance structure durable enough to survive disruption.
% FOUNDING_PROBLEM_CORROBORATION: Diaspora practitioners and outside ethnographers/survival researchers attest the practical-transmission function remains live where environmental instability recurs — this is corroboration from outside the elder/beneficiary seat. Ritual elders themselves assert the function is universally live regardless of setting, which is self-interested testimony given their standing depends on the claim; independent survey work comparing settled and displaced practice would be needed to adjudicate the contested middle ground.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__competence_transmission_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__competence_transmission_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__competence_transmission_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_survival__competence_transmission_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__competence_transmission_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.52 by interval end — this reading claims the ritual genuinely once encoded (and in diaspora settings still encodes) transferable survival competence, so the extraction is not the whole story the way it would be under a pure boundary-maintenance reading; the extraction is specifically the widening gap between settled communities' compliance cost and their practical benefit as crisis conditions recede. Theater ratio rises steeply (0.22 to 0.58) because as the practical content decouples from lived necessity in settled contexts, more of the ritual's persistence is carried by performance and correctness-policing rather than by anyone actually needing the resource-timing information. Suppression is moderate and rises slowly (0.28 to 0.44): the main suppressive force is identity-fusion among younger performers rather than external coercion, and it accumulates gradually as the practical rationale becomes less visible/teachable across generations.
 *
 * PERSPECTIVAL GAP:
 *   Diaspora communities and ritual elders would compute this constraint very differently from settled descendant communities and younger performers. From the diaspora seat, this is close to a rope: real coordination benefit, low suppression, workable exit. From the settled/younger performer seat, the same structure computes closer to extraction: high compliance cost, identity-locked exit, and a coordination story (practical competence) that increasingly does not cash out for their actual conditions. The engine should register this asymmetry as seat divergence rather than resolving it to one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual elders sit near the beneficiary end: they administer the content, hold interpretive authority, and derive standing from being correct readers of the calendar — d is low. Diaspora communities are also beneficiary-leaning: mobile exit, genuine adaptive payoff, low imposed cost — d is low-to-symmetric. Settled descendant communities and younger performers sit toward the target end: trapped or identity-locked exit, bearing the full compliance cost of a ceremonial apparatus whose payload has partly gone silent for them — d is high. This is exactly the beneficiary/victim split the kernel-contest expected for this reading: diaspora gains adaptive capacity, settled/young performers lose practical content while maintaining form.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transmitting operational survival knowledge across generational gaps) is genuinely dead for many settled descendant communities but the arrangement persists at full cost — this is the classic mandatrophy signature. Classifying this as tangled_rope rather than snare prevents mislabeling a structure with a real, still-partially-live coordination function (diaspora adaptation) as pure extraction; classifying it as tangled_rope rather than rope prevents pretending the settled-community cost asymmetry away. The rising theater_ratio is the leading indicator that the coordination function is atrophying faster in some populations than others — a within-kernel divergence this single reading is built to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    practical_content_survival_test,
    'Does the ritual''s embedded practical knowledge (timing rules, resource protocols) actually still transmit correctly in settled communities, or has it decayed into unreconstructable form while performers believe they are still transmitting content?',
    'Comparative ethnographic testing: have settled-community performers attempt to reconstruct the underlying resource-management logic from the ritual sequence alone, and compare against diaspora practitioners who have had to actively apply it; a large gap indicates content decay behind an intact form.',
    'If practical content has fully decayed in settled communities, this reading collapses toward the symbol_survival_reading''s territory for that population specifically — the tangled_rope classification would then depend entirely on the diaspora population to retain any coordination function, sharpening the victim/beneficiary asymmetry further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practical_content_survival_test, empirical, 'Whether settled-community ritual performance still carries recoverable practical content or only its shell.').

omega_variable(
    reading_boundary_which_kernel_claim_is_true,
    'Is the competence_transmission_reading the historically primary function of this ritual cycle, or is it a modern functionalist reinterpretation imposed retrospectively on a practice whose original and continuing purpose is symbolic/boundary-maintaining (the sibling symbol_survival_reading)?',
    'Historical-linguistic and archaeological cross-referencing: does the earliest recoverable form of the ritual encode specific, falsifiable operational content (precise timings tied to observable environmental signals) or primarily symbolic/kinship-marking content with practical detail added later or read in by outside analysts?',
    'If the practical-transmission function is a retrospective overlay rather than the ritual''s genealogical purpose, this reading''s claimed_type and beneficiary structure would need revision — the coordination function this story treats as historically load-bearing may be an artifact of functionalist analysis rather than the kernel''s own operative logic. This is the core committer-structure disagreement that distinguishes this reading from its two siblings and is deliberately NOT resolved within this story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_which_kernel_claim_is_true, conceptual, 'Whether practical-competence transmission is the ritual''s primary historical function or a modern interpretive overlay — the central axis separating this reading from symbol_survival_reading and hybrid_encoding_reading.').

omega_variable(
    elder_authority_capture_vs_genuine_expertise,
    'Is the elder/knowledge-holder class''s interpretive authority a genuine repository of decaying-but-real practical expertise, or has it become a self-perpetuating credentialing structure that polices ''correct'' performance regardless of whether it retains practical content?',
    'Track whether elder-endorsed ritual modifications track actual environmental/resource conditions (evidence of live expertise) or track internal succession and status competition among elders (evidence of credentialing capture).',
    'If elder authority has decoupled from practical expertise, the beneficiary designation for ritual_elders_and_knowledge_holders should be reconsidered as extraction riding on residual deference rather than coordination payoff — pushing the classification toward snare for that seat specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elder_authority_capture_vs_genuine_expertise, empirical, 'Whether elder interpretive authority still tracks genuine practical expertise or has become self-perpetuating credentialing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__competence_transmission_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 24, 0.46).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 32, 0.53).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cata_be_t8, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(cata_su_t8, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(cata_su_t16, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(cata_su_t24, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(cata_su_t32, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 32, 0.42).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 40, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__competence_transmission_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__competence_transmission_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the catastrophe_memory_survival kernel, each authored as a separate ε-invariant story per the decomposition principle. competence_transmission_reading (this story) treats practical knowledge transfer as primary and symbolic function as incidental, yielding moderate rising extraction concentrated on settled communities whose practical need has receded. symbol_survival_reading treats identity/boundary continuity as primary and practical content as incidental, which would be expected to show low extraction and a different beneficiary/victim map (continuity itself as the good, not adaptive capacity). hybrid_encoding_reading treats both registers as jointly necessary, which would be expected to average or hybridize the two extraction profiles rather than resolve cleanly to either. The three are linked bidirectionally via affects_constraints; none is privileged as 'the' correct reading of the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
