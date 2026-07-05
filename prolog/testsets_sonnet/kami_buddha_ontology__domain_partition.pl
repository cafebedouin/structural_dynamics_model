% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__domain_partition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__domain_partition, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: kami_buddha_ontology__domain_partition
 *   human_readable: Domain-Partition Reading of Kami-Buddha Ontology (Shinbutsu Bunri Function)
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   Historically, Japanese religious institutions distributed ritual labor
 *   between Shinto shrines (handling life passages: birth, coming-of-age,
 *   marriage, harvest, purity maintenance) and Buddhist temples (handling
 *   death: funerals, memorial rites, management of ancestor status),
 *   especially from roughly the Heian period through pre-Meiji
 *   shinbutsu-shugo. The domain-partition reading treats this as reflecting a
 *   real ontological boundary - kami and buddhas are different KINDS of
 *   entity suited to different registers of human life - rather than a
 *   disguised hierarchy (honji-suijaku) or an unprincipled accommodation
 *   (incoherent bundle). Under this reading the arrangement functions much
 *   like a genuine coordination mechanism: two specialist traditions serving
 *   complementary needs, with low overhead and no single authority forcing
 *   doctrinal unification.
 *
 * KEY AGENTS:
 *   - shrine_priests: agenda_setter/beneficiary (organized/regional) - control life-domain ritual, benefit from jurisdictional clarity
 *   - buddhist_death_ritual_specialists: agenda_setter/beneficiary (organized/regional) - control death-domain ritual, benefit from monopoly on funerary function
 *   - lay_households_managing_ritual_calendar: beneficiary/payer (moderate/local) - get legible division of labor at the cost of dual patronage obligation
 *   - theological_systematizers and honji_suijaku_theorists: excluded (powerless/analytical) - sidelined by a reading that does not require settling the metaphysical question they care about
 *   - comparative_religion_scholars: observer (analytical/global) - note all three kernel readings are defensible against the historical record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition, 0.28).
domain_priors:suppression_score(kami_buddha_ontology__domain_partition, 0.22).
domain_priors:theater_ratio(kami_buddha_ontology__domain_partition, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, extractiveness, 0.28).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__domain_partition, rope).
narrative_ontology:human_readable(kami_buddha_ontology__domain_partition, "Domain-Partition Reading of Kami-Buddha Ontology (Shinbutsu Bunri Function)").
narrative_ontology:topic_domain(kami_buddha_ontology__domain_partition, "religious_studies/philosophy_of_religion/japanese_cultural_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__domain_partition, '4bfdc7d1-7de3-46c4-b981-ed6951bdfe8e').
narrative_ontology:cs_kernel_codification('4bfdc7d1-7de3-46c4-b981-ed6951bdfe8e', distributed).
narrative_ontology:cs_authority_grounding('4bfdc7d1-7de3-46c4-b981-ed6951bdfe8e', practice).
narrative_ontology:cs_interpretation_layer_present('4bfdc7d1-7de3-46c4-b981-ed6951bdfe8e').
narrative_ontology:cs_reading_relation('4bfdc7d1-7de3-46c4-b981-ed6951bdfe8e', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('4bfdc7d1-7de3-46c4-b981-ed6951bdfe8e', kami_buddha_ontology__incoherent_bundle, influences).
narrative_ontology:cs_axiom('4bfdc7d1-7de3-46c4-b981-ed6951bdfe8e', foundational, kami_and_buddhas_ontologically_distinct_kinds).
narrative_ontology:cs_axiom_status(kami_and_buddhas_ontologically_distinct_kinds, holdable).
narrative_ontology:cs_axiom_grounding('4bfdc7d1-7de3-46c4-b981-ed6951bdfe8e', kami_and_buddhas_ontologically_distinct_kinds, conventional).
narrative_ontology:cs_axiom('4bfdc7d1-7de3-46c4-b981-ed6951bdfe8e', foundational, functional_domains_non_overlapping_without_hierarchy).
narrative_ontology:cs_axiom_status(functional_domains_non_overlapping_without_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('4bfdc7d1-7de3-46c4-b981-ed6951bdfe8e', functional_domains_non_overlapping_without_hierarchy, conventional).
narrative_ontology:cs_reference_frame('4bfdc7d1-7de3-46c4-b981-ed6951bdfe8e', heian_era_jingu_ji_complex_practice).
narrative_ontology:cs_drift_state('4bfdc7d1-7de3-46c4-b981-ed6951bdfe8e', meiji_shinbutsu_bunri, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('4bfdc7d1-7de3-46c4-b981-ed6951bdfe8e', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__domain_partition, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, shrine_priests).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, buddhist_death_ritual_specialists).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, lay_households_managing_ritual_calendar).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition, lay_households_managing_ritual_calendar).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__domain_partition, ontological_dualism_of_kami_and_buddhas).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__domain_partition, functional_non_overlap_of_purity_and_impurity_domains).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer kami worship for birth, growth, marriage, harvest, and community purity rites. Under the domain-partition reading, their jurisdiction is theoretically clean: death and its pollution (kegare) are categorically outside their remit and are handed to Buddhist specialists, which protects the ritual purity that is the source of their institutional authority. They benefit from a clear boundary that neither obligates them to handle death pollution nor cedes life-affirming ritual ground to Buddhist institutions.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, shrine_priests, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__domain_partition, shrine_priests, beneficiary).

% Administer funerary rites, memorial services, and management of the deceased's continuing status. The domain-partition reading grants them uncontested monopoly over death and its aftermath, in exchange for staying out of the kami's jurisdiction over living/purity matters. This is a durable revenue and status base (temple registration systems, funeral fees, memorial rites) that depends on death remaining categorically Buddhist territory.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, buddhist_death_ritual_specialists, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__domain_partition, buddhist_death_ritual_specialists, beneficiary).

% Rely on a legible division of ritual labor: shrine for births and life-passages, temple for funerals and ancestor rites. This partition lets a household know, without theological training, which institution to approach for which need. The cost is being locked into patronizing both institutions across a lifecycle, with no institution offering a single, unified account of what a kami or a buddha actually is or how the two relate.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, lay_households_managing_ritual_calendar, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__domain_partition, lay_households_managing_ritual_calendar, payer).

% Scholars and clerics who want a single coherent metaphysics explaining why kami and buddhas relate as they do are structurally sidelined by this reading, which is satisfied with functional non-overlap and does not require (or want) a unifying doctrine. Their objection - that a partition without ontological grounding is just an administrative convenience dressed as metaphysics - has no seat in the arrangement as practiced.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, theological_systematizers, excluded,
    powerless, civilizational, analytical, national).

% Hold the rival reading that kami are manifestations of buddhas and are excluded from this constraint's own operation, since the domain-partition reading does not require or engage their honji-suijaku framework at all. They are not persecuted, simply irrelevant to how this particular reading justifies itself.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, honji_suijaku_theorists, excluded,
    powerless, generational, analytical, national).

% Study shinbutsu-shugo as a case of religious syncretism and note that the domain-partition account, the honji-suijaku account, and the incoherent-bundle account are all defensible readings of the same historical record, none dispositively refuted by the sources, which is itself the interesting datum.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides two institutions (shrine and temple) with a non-competing division of ritual labor across the human lifecycle, so that life-affirming and death-related ritual needs are each handled by a specialist tradition without either institution having to develop competence in, or dispute jurisdiction over, the other's domain.
% TRANSFER_FUNCTION: Moves ritual patronage, fees, and social obligation from households to two separate institutions according to life-stage: birth/growth/marriage/purity rites flow to shrines, death/funerary/memorial rites flow to temples. No wealth or status transfer occurs between the two institutions themselves under this reading - the domains simply do not touch.
% ABSENT_VOICES: Systematizing theologians and honji-suijaku theorists who want (or already hold) an account of how kami and buddhas relate metaphysically are not represented in the domain-partition arrangement's own self-justification, since the reading's coherence does not require settling that question - it treats the two as simply different kinds of thing operating in different registers.
% DISAPPEARANCE_RATIONALE: If the domain-partition reading vanished as an operative account, the underlying institutional division of ritual labor (shrines for life, temples for death) would likely persist as sociological fact - it survived the Meiji shinbutsu bunri (forced separation) and continues informally in most Japanese households today. What would change is the THEORETICAL justification: without this reading, the same practical split would need re-grounding, either in honji-suijaku monism, in the incoherent-bundle account, or in some new synthesis. Whether 'the world rearranges' therefore depends on whether one is asking about ritual practice (largely unchanged) or about theological self-understanding (significantly disrupted) - hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: Medieval Japanese religious life needed a way to accommodate two textually and doctrinally distinct traditions - indigenous kami cults and imported Buddhism - operating in the same communities, often at the same shrine-temple complexes, without either tradition being absorbed or eliminated, and without requiring ordinary worshippers to resolve a metaphysical dispute before performing a wedding or a funeral.
% FOUNDING_PROBLEM_CORROBORATION: Shrine priests and temple clergy (the benefiting parties) both attest the partition remains functionally necessary. Outside corroboration is mixed: Meiji-era state Shinto ideologues attest the partition was real enough to be forcibly legislated (shinbutsu hanzenrei, 1868) precisely because prior praxis had NOT kept the domains clean - suggesting the partition reading may retroactively tidy a messier historical bundle. Comparative religion scholars (an outside seat) generally treat domain-partition as one defensible reading among the three in contest, not as an uncontested historical description.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__domain_partition, contested).
narrative_ontology:founding_problem_status(kami_buddha_ontology__domain_partition, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__domain_partition, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kami_buddha_ontology__domain_partition, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__domain_partition, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__domain_partition_tests).
:- end_tests(kami_buddha_ontology__domain_partition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.28) and mostly rises through gradual institutional consolidation (temple registration systems formalizing funerary fees, shrine associations formalizing purity-rite fees) rather than through any coercive mechanism - this is closer to genuine coordination cost than rent extraction. Suppression is low (0.22): households are not coerced into the partition by force: the boundary persists because it is practically convenient and doctrinally undemanding, not because alternatives are blocked. Theater ratio stays low throughout (0.15 at end) because the division of labor does real ritual work; there is little performative excess. Accessibility collapse is moderate (0.35): households could in principle patronize either institution for either function (some Buddhist funerary elements appear at shrines historically and vice versa in local practice), so the partition is a strong norm, not an iron boundary - this keeps the reading closer to rope than to mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Shrine priests and Buddhist death specialists are both near the beneficiary end: each retains uncontested jurisdiction over half the ritual lifecycle without having to compete with or absorb the other institution's specialized knowledge, personnel, or symbolic capital. Lay households sit closer to symmetric - they get a legible, low-cognitive-cost map of which institution to approach, but pay into both systems across a lifetime with no unified account of what they are ultimately relating to ontologically. There are no true victims under this reading: no group is structurally harmed by the domain partition itself (which is why base_properties.victims is empty and requires_active_enforcement is false) - this is a key structural marker distinguishing domain_partition from the other kernel readings, where hierarchy (honji_suijaku) or forced contradiction-management (incoherent_bundle) could generate identifiable losers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - accommodating two traditions without requiring doctrinal reconciliation - remains at least partially live: Japan continues to sustain institutionally separate shrine and temple systems serving different ritual registers, centuries after Meiji's forced administrative shinbutsu bunri. The founding_problem_status is authored as contested rather than dead because the Meiji-era forced separation (shinbutsu hanzenrei) is itself evidence that the 'natural' partition this reading describes was, at a minimum, in need of state enforcement to become administratively clean - suggesting either that the partition was already substantially real (supporting this reading) or that it required construction (supporting the incoherent_bundle reading). The domain-partition classification here does not resolve that tension; it names one coherent way of reading the historical record, structurally distinct from its siblings, and lets the divergence between claim and metric register rather than forcing resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Is domain_partition the historically dominant self-understanding of shinbutsu-shugo practitioners, or is it a retrospective scholarly tidying imposed on a messier lived reality that more closely resembled the incoherent_bundle reading?',
    'Systematic textual analysis of pre-Meiji shrine-temple complex (jingu-ji) administrative records and ritual manuals across regions and periods, checking whether local practitioners articulated a clean domain boundary or operated with simultaneous, unreconciled commitments to fusion (honji-suijaku) and separation as convenient.',
    'If lived practice was predominantly domain-partition-consistent, this reading has strong claim to historical priority; if practice shows persistent, unremarked contradiction (kami treated as both distinct AND as buddha-traces in the same complex, same period), the incoherent_bundle reading better describes the actual kernel and domain_partition is a modern rationalization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, empirical, 'Whether domain-partition was the operative historical self-understanding or a later rationalization of bundled contradictions.').

omega_variable(
    purity_impurity_metaphysical_ground,
    'Does the life/death, purity/impurity boundary between kami and buddha jurisdiction reflect a genuine ontological distinction between two kinds of sacred entity, or is it a practical ritual-hygiene convention (keeping death pollution away from kami shrines) that has been read back into the entities'' natures?',
    'Comparative analysis of whether kami are treated as categorically incapable of engaging death-related matters in contexts without institutional Buddhist competition (e.g., isolated shrine traditions with minimal historical Buddhist contact), versus contexts of direct shrine-temple competition where the boundary might be functionally motivated.',
    'If the purity boundary is independently attested outside competitive contexts, this reading''s ontological claim strengthens; if the boundary only appears where shrine and temple institutions coexist and compete, the ''ontological distinctness'' may be institutional convenience dressed as metaphysics - weakening domain_partition relative to incoherent_bundle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(purity_impurity_metaphysical_ground, conceptual, 'Whether the kami/buddha functional split reflects genuine ontology or institutionally-motivated ritual convention.').

omega_variable(
    false_summit_check_vindicated_dualism,
    'Do the declared beneficiaries (shrine priests, Buddhist specialists) benefit BECAUSE the ontological dualism is true, or does the institutional benefit exist independently and the dualism is presented as natural/given to avoid scrutiny of the jurisdictional arrangement''s constructedness?',
    'Examine whether shrine and temple institutions historically resisted or embraced periods when the boundary blurred (e.g., syncretic ritual innovation, shared clergy) - resistance to blurring would suggest institutional interest is doing real work independent of the ontological claim''s truth.',
    'If institutions actively defended the boundary against blurring even when doctrinally permissive alternatives were available, this supports reading the ''ontological distinctness'' claim as partly serving institutional interest rather than being purely descriptive - relevant because this constraint is claimed as rope but declares beneficiaries, which is the FSM-adjacent condition the schema flags for scrutiny even though this is not authored as a mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_check_vindicated_dualism, conceptual, 'Whether beneficiary institutions have independent interest in maintaining the ontological-dualism claim regardless of its truth.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__domain_partition, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__domain_partition, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(kami_tr_t0, projected).
narrative_ontology:measurement(kami_tr_t200, kami_buddha_ontology__domain_partition, theater_ratio, 200, 0.11).
narrative_ontology:measurement_basis(kami_tr_t200, projected).
narrative_ontology:measurement(kami_tr_t400, kami_buddha_ontology__domain_partition, theater_ratio, 400, 0.12).
narrative_ontology:measurement_basis(kami_tr_t400, projected).
narrative_ontology:measurement(kami_tr_t600, kami_buddha_ontology__domain_partition, theater_ratio, 600, 0.13).
narrative_ontology:measurement_basis(kami_tr_t600, observed).
narrative_ontology:measurement(kami_tr_t800, kami_buddha_ontology__domain_partition, theater_ratio, 800, 0.14).
narrative_ontology:measurement_basis(kami_tr_t800, observed).
narrative_ontology:measurement(kami_tr_t1000, kami_buddha_ontology__domain_partition, theater_ratio, 1000, 0.14).
narrative_ontology:measurement_basis(kami_tr_t1000, observed).
narrative_ontology:measurement(kami_tr_t1200, kami_buddha_ontology__domain_partition, theater_ratio, 1200, 0.15).
narrative_ontology:measurement_basis(kami_tr_t1200, observed).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__domain_partition, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(kami_be_t0, projected).
narrative_ontology:measurement(kami_be_t200, kami_buddha_ontology__domain_partition, base_extractiveness, 200, 0.2).
narrative_ontology:measurement_basis(kami_be_t200, projected).
narrative_ontology:measurement(kami_be_t400, kami_buddha_ontology__domain_partition, base_extractiveness, 400, 0.22).
narrative_ontology:measurement_basis(kami_be_t400, projected).
narrative_ontology:measurement(kami_be_t600, kami_buddha_ontology__domain_partition, base_extractiveness, 600, 0.24).
narrative_ontology:measurement_basis(kami_be_t600, observed).
narrative_ontology:measurement(kami_be_t800, kami_buddha_ontology__domain_partition, base_extractiveness, 800, 0.26).
narrative_ontology:measurement_basis(kami_be_t800, observed).
narrative_ontology:measurement(kami_be_t1000, kami_buddha_ontology__domain_partition, base_extractiveness, 1000, 0.27).
narrative_ontology:measurement_basis(kami_be_t1000, observed).
narrative_ontology:measurement(kami_be_t1200, kami_buddha_ontology__domain_partition, base_extractiveness, 1200, 0.28).
narrative_ontology:measurement_basis(kami_be_t1200, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kami_buddha_ontology__domain_partition, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__domain_partition, identity_coordination).
narrative_ontology:boltzmann_floor_override(kami_buddha_ontology__domain_partition, 0.1).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, kami_buddha_ontology__honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, kami_buddha_ontology__incoherent_bundle).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the kami_buddha_ontology kernel. domain_partition (this story) treats kami and buddhas as two ontologically distinct entity-types with non-overlapping functional jurisdiction and low extraction/suppression (rope-leaning). honji_suijaku_monism treats kami as phenomenal traces of an underlying buddha-ground, introducing hierarchy (buddhas as honji/ground, kami as suijaku/trace) with correspondingly different beneficiary structure (Buddhist institutional primacy) and likely higher extraction given the subordination claim. incoherent_bundle treats the entire shinbutsu-shugo complex as an institutionally sustained set of contradictory commitments held simultaneously without resolution, which would classify very differently (likely tangled_rope or piton, given the bundle's dependence on NOT resolving its internal contradictions). All three share the same historical substrate (medieval-through-Meiji Japanese shrine-temple institutional practice) but represent structurally distinct ontological claims with different ε profiles, per the ε-invariance principle - hence three separate stories rather than one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
