% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__behavioral_competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: tsunami_stone_commitment__behavioral_competence_reading
 *   human_readable: Tsunami Stone Commitment—Behavioral Competence Reading
 *   domain: institutional/anthropological
 *
 * SUMMARY:
 *   Tsunami stone inscriptions inscribed on rocks in coastal Japanese
 *   communities over centuries encode warnings: 'Do not build below this
 *   point' or variants. Under the behavioral-competence reading, these stones
 *   retained active normative force—elders taught children to recognize the
 *   markers and evacuate when natural signs of tsunami appeared (receding
 *   tide, earth shaking). The norm was enforced intergenerationally through
 *   familial and community transmission, not through coercive apparatus. The
 *   2011 Tōhoku tsunami struck; communities with active knowledge of the
 *   stones had significantly lower mortality rates than those that had let
 *   the practice fade into commemorative symbolism. This reading treats the
 *   constraint as a successfully stabilized rope: genuine coordination
 *   problem (how to preserve lifesaving knowledge across generations when
 *   individual disaster experience is rare and exponentially decay), solved
 *   through material inscription and social transmission, minimal extraction,
 *   no concentrated beneficiary. The sibling commemorative-husk reading
 *   treats the same kernel (the stones, the historical practice) as having
 *   atrophied into symbolic artifact by 2011—compliance was coincidental or
 *   weakly enforced, and the 2011 outcome vindicated the original design but
 *   not its operational status at the measurement time.
 *
 * KEY AGENTS:
 *   - coastal_communities: bearers and practitioners of the transmission tradition; teach or do not teach children the meaning of the stones
 *   - regional_administrators: historically maintained the tradition; codified and publicized stone locations in the modern era
 *   - families_with_intergenerational_knowledge: preserve the practice through oral and embodied teaching
 *   - families_without_active_knowledge: treat stones as historical monuments, if they engage with them at all
 *   - analytical_observer: reconstructs pre-2011 behavioral status and 2011 outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__behavioral_competence_reading, 0.08).
domain_priors:suppression_score(tsunami_stone_commitment__behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(tsunami_stone_commitment__behavioral_competence_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__behavioral_competence_reading, rope).
narrative_ontology:human_readable(tsunami_stone_commitment__behavioral_competence_reading, "Tsunami Stone Commitment—Behavioral Competence Reading").
narrative_ontology:topic_domain(tsunami_stone_commitment__behavioral_competence_reading, "institutional/anthropological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__behavioral_competence_reading, 'adc0293c-96fa-4d01-96cd-e2e3901dacac').
narrative_ontology:cs_kernel_codification('adc0293c-96fa-4d01-96cd-e2e3901dacac', fixed_text).
narrative_ontology:cs_authority_grounding('adc0293c-96fa-4d01-96cd-e2e3901dacac', lineage).
narrative_ontology:cs_interpretation_layer_present('adc0293c-96fa-4d01-96cd-e2e3901dacac').
narrative_ontology:cs_reading_relation('adc0293c-96fa-4d01-96cd-e2e3901dacac', tsunami_stone_commitment__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('adc0293c-96fa-4d01-96cd-e2e3901dacac', tsunami_stone_commitment__catastrophe_validation_axis, influences).
narrative_ontology:cs_axiom('adc0293c-96fa-4d01-96cd-e2e3901dacac', foundational, intergenerational_transmission_remains_behaviorally_active).
narrative_ontology:cs_axiom_status(intergenerational_transmission_remains_behaviorally_active, holdable).
narrative_ontology:cs_axiom_grounding('adc0293c-96fa-4d01-96cd-e2e3901dacac', intergenerational_transmission_remains_behaviorally_active, empirically_contingent).
narrative_ontology:cs_axiom('adc0293c-96fa-4d01-96cd-e2e3901dacac', secondary, material_inscription_anchors_institutional_memory).
narrative_ontology:cs_axiom_status(material_inscription_anchors_institutional_memory, holdable).
narrative_ontology:cs_axiom_grounding('adc0293c-96fa-4d01-96cd-e2e3901dacac', material_inscription_anchors_institutional_memory, deontological).
narrative_ontology:cs_reference_frame('adc0293c-96fa-4d01-96cd-e2e3901dacac', active_intergenerational_transmission).
narrative_ontology:cs_drift_state('adc0293c-96fa-4d01-96cd-e2e3901dacac', modern_era_pre_2011, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('adc0293c-96fa-4d01-96cd-e2e3901dacac', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, coastal_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, families_with_active_knowledge).
narrative_ontology:constraint_victim(tsunami_stone_commitment__behavioral_competence_reading, families_with_active_knowledge).
narrative_ontology:constraint_victim(tsunami_stone_commitment__behavioral_competence_reading, families_without_active_knowledge).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Preserve and transmit knowledge of tsunami hazard signs encoded in stone inscriptions. Families teach children to recognize the markers and evacuate; the teaching is embedded in cultural practice and local geography education. They benefit from the knowledge transmission without bearing extraction costs. In 2011, communities with active transmission of the stone-inscription knowledge had significantly lower mortality rates, validating the coordination function.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, coastal_communities, beneficiary,
    organized, generational, mobile, regional).

% Maintain intergenerational teaching of tsunami recognition and evacuation. The teaching burden is distributed—each family bears the cost of instructing children and modeling compliance—but the benefit is survival of the community. Exit from the practice means accepting higher risk; the constraint is enforced through social norms and family expectation, not external apparatus.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, families_with_active_knowledge, payer,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__behavioral_competence_reading, families_with_active_knowledge, beneficiary).

% Have allowed intergenerational transmission to lapse; treat the stones as historical monuments rather than active safety instructions. They rely on modern evacuation sirens and official warnings. Their exit from the traditional practice was gradual and unmarked—the knowledge faded as modernization displaced traditional transmission methods. Re-entry would require learning from communities that preserved the practice or from archaeological/anthropological research.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, families_without_active_knowledge, payer,
    moderate, biographical, identity_locked, local).

% Historically maintained records of stone locations and, in the modern era, formalized them as official cultural heritage and disaster-preparedness markers. They publicize the stones, install plaques, educate visitors, coordinate with communities to preserve the tradition. They do not extract from the arrangement; they maintain its public visibility and institutional memory.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, regional_administrators, agenda_setter,
    institutional, generational, analytical, regional).

% Reconstructs the pre-2011 behavioral status of the tradition, analyzes the 2011 outcome, and assesses whether the stones retained active behavioral force or had become symbolic artifacts. Reads ethnographic and historical sources, compares survival outcomes across communities.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tsunami_stone_commitment__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(tsunami_stone_commitment__behavioral_competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserving knowledge of rare, catastrophic natural events across generations—specifically, the signs that precede tsunamis and the proper response (evacuate to high ground). Individual memory cannot span centuries; disasters are rare enough that living experience is unreliable. The stones encode knowledge in durable material; intergenerational transmission keeps the knowledge alive.
% TRANSFER_FUNCTION: No transfer of goods or extraction of value. Knowledge flows from elders to children; the constraint is non-extractive coordination, not a transfer mechanism.
% ABSENT_VOICES: Modern coastal developers who would prefer to build in low-lying zones without ancestral evacuation constraints would object if they were heard. Scientific authorities who dispute the specific markers or recommend alternative evacuation models would contest the stones' accuracy. Inland communities unaffected by tsunami have no stake. These voices are structurally absent from the traditional community transmission but present in modern governance debates.
% DISAPPEARANCE_RATIONALE: If the stone inscriptions and their intergenerational transmission vanished overnight, coastal communities would lose a channel of knowledge transmission. Modern sirens and official evacuation protocols would remain, but the embodied, culturally-grounded understanding of natural danger signs would be compromised. Families that preserved active knowledge would suddenly have no physical or social infrastructure for teaching; the cultural practice would fragment. The 2011 data suggests that communities with active knowledge had superior outcomes, so disappearance of the constraint would entail loss of a historically validated coordination mechanism.
% FOUNDING_PROBLEM: In the pre-modern era, coastal communities needed to transmit knowledge of tsunami hazards across generations despite the exponential rarity of lived disaster experience. Without written mass media or institutional disaster planning, the only transmission mechanism was oral tradition and material marking. The stones were inscribed to anchor the knowledge in durable, geographically-embedded form.
% FOUNDING_PROBLEM_CORROBORATION: Communities that preserved active transmission attest the founding problem remains live—they actively teach children the meaning of the stones. The 2011 Tōhoku tsunami provides empirical validation from outside the benefiting parties: independent analysis of mortality rates by community and knowledge-preservation status shows communities with active transmission had significantly lower casualties. However, some communities and modern administrators treat the stones primarily as historical monuments, suggesting the founding problem has partially atrophied to symbolic concern. The preponderance of modern evidence (sirens, formal evacuation protocols, educational institutionalization) suggests the original problem is now addressed by alternative means, though the stones remain functionally effective where active transmission persists.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__behavioral_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tsunami_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__behavioral_competence_reading, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08 average) because there is no identifiable beneficiary capturing gains from the constraint's operation—it is neither market extraction (no transfer mechanism), nor institutional rent (no bureaucracy collecting), nor identity lock (the knowledge is valuable, not coercive). Coastal communities benefit from preserved safety knowledge, but they are not beneficiaries-in-the-sense-of-extractive-structures; they are co-producers of a coordination solution. Suppression is minimal (0.12) because the norm is primarily internalized—families pass knowledge because they believe it works and matters, not because they fear punishment for non-compliance. Theater is low-moderate (0.18 average, rising slightly over time as modern formalization begins—museums, official markers, tourism) because the core practice is functional transmission, not performative maintenance. Accessibility-collapse is very high (0.92) because once a family loses the knowledge (it is not written down in most households, it is embodied practice), re-discovering the stones' meaning from inscription alone is difficult without community guidance—alternatives (building where marked, ignoring evacuation signals) are strongly discouraged by practical outcome. Resistance is minimal (0.05) because the norm is not contentious—no party disputes that preserving tsunami knowledge is good. The measurement series shows slight upward drift in theater_ratio starting around t=50 (corresponding to 1960s–1980s modernization of coastal Japan, formalization of disaster planning, museum-ification of stones), but extractiveness remains flat and low, supporting the piton-framing risk: the theater rises as function atrophies, but extraction never rises because there was no beneficial renter to begin with. This reading claims rope (genuine coordination, minimal overhead) while authored metrics are consistent with either rope or piton (depending on whether intergenerational transmission remained behaviorally active or had degraded to symbolic inertia)—the engine measures this divergence.
 *
 * PERSPECTIVAL GAP:
 *   Under the behavioral-competence reading, the community seat and the analytical seat compute identically—both see a coordination solution. Under the sibling commemorative-husk reading, the same stakeholders would compute differently: the community that treats stones as monuments would compute as operating under piton inertia, while the analytical seat would read the same constraint as a degraded rope. The two readings are not observer-relative perspectives on the same type; they are different structural claims about whether active transmission persists.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal communities are symmetrically positioned: they benefit from knowledge transmission (no cost, survival value), they sustain transmission (teaching burden is diffuse, intergenerational), no one collects extraction. d ≈ 0.5 for all seats—there is no target, no beneficiary-in-the-extractive-sense. Directionality overrides are unnecessary; the beneficiary declaration ('coastal_communities') refers to coordination benefit, not extractive capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—how to preserve knowledge of rare, catastrophic events across generations when individual memory cannot span centuries—is live in this reading: the 2011 outcome demonstrates the stones still carried behavioral force sufficient to improve survival outcomes. Under the commemorative-husk reading, the founding problem is dead (the stones persist out of inertia, not because transmission is active), and mandatrophy would be declared. The engine's mandatrophy gate checks whether founding_problem_status='dead' and disappearance_verdict='world_rearranges'—a mismatch that flags zombie constraints. Here, founding_problem_status='live', so the gate does not fire even if the theater_ratio measurement shows slight performative inflation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_vs_commemorative_boundary,
    'Did the stone inscriptions retain active behavioral force (norm enforcement, intergenerational transmission of evacuation knowledge) or had they degraded to symbolic artifacts whose compliance in 2011 was coincidental or weakly enforced?',
    'Ethnographic and historical reconstruction: pre-2011 community interviews on whether stone knowledge was actively taught, whether violations (building in marked zones) incurred social enforcement, whether the practice was alive or theatrical. Post-2011 analysis of whether communities that preserved active transmission had different survival outcomes than those treating stones as historical monuments.',
    'If behavioral (this reading): the constraint is a rope with minimal extractive overhead—institutional memory as coordination. Computed type should be rope or piton (depending on whether enforcement is still active). If commemorative (sibling reading): constraint is piton—the function atrophied, theaters persist, the 2011 outcome vindicates the original design but not its operational status at the time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_vs_commemorative_boundary, empirical, 'Whether the reading captures active norm enforcement or symbolic residue.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the kernel of this constraint the stone inscriptions themselves (the fixed material artifact), the practice of transmission they instantiate, or the commitment to coastal safety they encode?',
    'Clarification of what the authority structure—the lineage of coastal communities and regional administrators—treats as authoritative. If the stone is the kernel (fixed text, not revisable), then alternative transmission media or updated warnings would foreclose this reading. If the practice is the kernel, the stone is one exemplification among many. If the commitment is the kernel, both stone and practice are instruments.',
    'Different kernels produce different drift states: a stone-as-kernel reading flags codification collapse if the stone degrades or is ignored (strong drift signal); a practice-as-kernel reading flags practice drift if transmission methods change; a commitment-as-kernel reading focuses on whether the safety commitment persists via any means.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'What element is the kernel—the material artifact, the transmission practice, or the encoded commitment?').

omega_variable(
    suppression_mechanism_internalization,
    'To what extent is compliance with tsunami-avoidance norms structural (enforced by geography, institutional rule, social shame) versus internalized (agents have fused their identity with coastal safety practices)?',
    'Post-2011 community testimony: did agents evacuate because the stone commanded it, because they expected enforcement if they didn''t, or because they had internalized the value—did the instruction feel external or self-evident? Comparison across communities with different transmission histories.',
    'If mostly internalized, suppression measured at the constraint''s peak should be low (0.05–0.15), because the norm is self-maintaining without coercive apparatus. If structural, higher suppression is needed to sustain compliance; the measured low suppression (0.12) suggests internalization is dominant, supporting the rope reading (coordination, not extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether compliance is structurally enforced or internalized.').

omega_variable(
    id_2011_tsunami_vindication_timing,
    'Does the 2011 tsunami outcome (lower death toll in communities with active stone-inscription knowledge) vindicate the constraint as designed, or as it actually operated in 2011? Did the communities that survived best preserve active transmission or treat the stones commemoratively yet still benefit from their existence?',
    'Comparative analysis of 2011 survival rates by community, controlling for geographic elevation, prior evacuation experience, and active vs. symbolic knowledge of the stone inscriptions. Interview-based reconstruction of which communities actively taught children to evacuate when they see certain natural signs, versus treating the stones as historical.',
    'If active transmission → better outcomes: the behavioral reading is empirically vindicated. If symbolic treatment → equally good outcomes: the constraint persists through institutional inertia (piton) rather than through behavioral competence (rope). Mixed results would support the contested reading in six_questions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(id_2011_tsunami_vindication_timing, empirical, 'Whether the 2011 outcome vindicates active behavioral transmission or symbolic persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__behavioral_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t0, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(tsun_tr_t25, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 25, 0.14).
narrative_ontology:measurement(tsun_tr_t50, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 50, 0.16).
narrative_ontology:measurement(tsun_tr_t75, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 75, 0.18).
narrative_ontology:measurement(tsun_tr_t100, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 100, 0.18).

% Extraction over time
narrative_ontology:measurement(tsun_be_t0, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(tsun_be_t25, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 25, 0.07).
narrative_ontology:measurement(tsun_be_t50, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(tsun_be_t75, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 75, 0.09).
narrative_ontology:measurement(tsun_be_t100, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 100, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(tsunami_stone_commitment__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__behavioral_competence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(tsunami_stone_commitment__behavioral_competence_reading, 0.12).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__commemorative_husk_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__catastrophe_validation_axis).

% DUAL FORMULATION NOTE:
% The tsunami stone commitment decomposes into three constraint stories: this behavioral-competence reading (live intergenerational transmission, rope/piton depending on measured theater), the commemorative-husk reading (symbols persist, piton), and the catastrophe-validation-axis reading (the 2011 outcome as binary empirical test). The three readings share the kernel (the stones, the historical practice) but instantiate different constraints because they make different structural claims about what force the stones actually exert. They are linked via network.affects_constraints because the behavioral status of the transmission directly affects the explanatory power of the 2011 validation evidence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
