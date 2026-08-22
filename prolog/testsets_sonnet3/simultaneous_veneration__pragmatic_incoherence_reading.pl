% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__pragmatic_incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__pragmatic_incoherence_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: simultaneous_veneration__pragmatic_incoherence_reading
 *   human_readable: Shinbutsu-Shugo as Suppressed Doctrinal Incoherence (Pragmatic Incoherence Reading)
 *   domain: religious/historical
 *
 * SUMMARY:
 *   This story instantiates the pragmatic-incoherence reading of the
 *   shinbutsu-shugo kernel: pre-Meiji simultaneous veneration of kami and
 *   buddhas was never a resolved theological synthesis, but an
 *   institutionally convenient non-resolution. Combinatory shrine-temple
 *   complexes (jingu-ji) administered joint rites, collected joint revenue,
 *   and used honji-suijaku vocabulary as institutional cover, while lay
 *   practitioners and village communities absorbed the unexamined cost of
 *   holding two unreconciled cosmologies without any doctrinal forum to
 *   question it. On this reading, the 1868 Meiji shinbutsu-bunri edicts did
 *   not impose an artificial rupture on a stable synthesis — they revealed
 *   and forcibly resolved a latent incoherence that institutional actors had
 *   every incentive to leave unexamined for centuries. This is one reading
 *   among three of the same historical kernel; the ontological_fusion_reading
 *   holds honji-suijaku captured genuine metaphysical identity, and the
 *   domain_partition_reading holds the two traditions were functionally
 *   specialized rather than incoherent. Each is authored as its own
 *   constraint story with its own ε; this story's ε is high because, by this
 *   reading's own lights, what looks like coordination is substantially
 *   suppressed contradiction sustained by absence of enforcement rather than
 *   by doctrinal resolution.
 *
 * KEY AGENTS:
 *   - shrine_temple_administrative_complexes: primary agenda-setter and beneficiary — institutional (arbitrage) — captures consolidated revenue and authority from unresolved duality
 *   - buddhist_clergy_managing_shrine_precincts: beneficiary — organized (arbitrage) — dual role never required to reconcile doctrine
 *   - lay_practitioners_seeking_doctrinal_coherence: primary target — powerless (trapped) — bears cost of unexamined contradiction
 *   - reform_minded_kokugaku_scholars: dissenting payer/excluded — moderate (constrained) — paid social cost for raising the incoherence question early
 *   - meiji_state_shinto_architects: successor agenda-setter/beneficiary — institutional (arbitrage) — repurposed the revealed incoherence for new nationalist ends
 *   - comparative_religion_historians: analytical observer — assesses whether synthesis, partition, or suppressed contradiction best describes the historical record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, 0.71).
domain_priors:suppression_score(simultaneous_veneration__pragmatic_incoherence_reading, 0.58).
domain_priors:theater_ratio(simultaneous_veneration__pragmatic_incoherence_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__pragmatic_incoherence_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__pragmatic_incoherence_reading, "Shinbutsu-Shugo as Suppressed Doctrinal Incoherence (Pragmatic Incoherence Reading)").
narrative_ontology:topic_domain(simultaneous_veneration__pragmatic_incoherence_reading, "religious/historical").

domain_priors:requires_active_enforcement(simultaneous_veneration__pragmatic_incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__pragmatic_incoherence_reading, 'a01cca6a-a9b6-440f-9c62-a0d7107d408a').
narrative_ontology:cs_kernel_codification('a01cca6a-a9b6-440f-9c62-a0d7107d408a', distributed).
narrative_ontology:cs_authority_grounding('a01cca6a-a9b6-440f-9c62-a0d7107d408a', practice).
narrative_ontology:cs_interpretation_layer_present('a01cca6a-a9b6-440f-9c62-a0d7107d408a').
narrative_ontology:cs_reading_relation('a01cca6a-a9b6-440f-9c62-a0d7107d408a', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('a01cca6a-a9b6-440f-9c62-a0d7107d408a', simultaneous_veneration__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('a01cca6a-a9b6-440f-9c62-a0d7107d408a', foundational, unenforced_contradiction_is_not_resolution).
narrative_ontology:cs_axiom_status(unenforced_contradiction_is_not_resolution, holdable).
narrative_ontology:cs_axiom_grounding('a01cca6a-a9b6-440f-9c62-a0d7107d408a', unenforced_contradiction_is_not_resolution, empirically_contingent).
narrative_ontology:cs_axiom('a01cca6a-a9b6-440f-9c62-a0d7107d408a', secondary, institutional_convenience_can_masquerade_as_doctrine).
narrative_ontology:cs_axiom_status(institutional_convenience_can_masquerade_as_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('a01cca6a-a9b6-440f-9c62-a0d7107d408a', institutional_convenience_can_masquerade_as_doctrine, conventional).
narrative_ontology:cs_reference_frame('a01cca6a-a9b6-440f-9c62-a0d7107d408a', pre_meiji_combinatory_practice_as_unexamined_default).
narrative_ontology:cs_drift_state('a01cca6a-a9b6-440f-9c62-a0d7107d408a', meiji_shinbutsu_bunri_1868, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('a01cca6a-a9b6-440f-9c62-a0d7107d408a', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, shrine_temple_administrative_complexes).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, buddhist_clergy_managing_shrine_precincts).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, local_kami_priests_retaining_temple_patronage).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, lay_practitioners_seeking_doctrinal_coherence).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, reform_minded_kokugaku_scholars).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, village_communities_bearing_dual_ritual_obligations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, meiji_state_shinto_architects).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, local_kami_priests_retaining_temple_patronage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manages the combined jingu-ji institutions that hold land, ritual authority, and revenue jointly across kami shrines and Buddhist temple precincts. Administers rites that invoke both kami and buddhas without requiring practitioners or clergy to reconcile the underlying metaphysics, and benefits from the ambiguity because it preserves consolidated landholding and dual revenue streams (shrine offerings plus temple patronage).
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, shrine_temple_administrative_complexes, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Buddhist priests who also administer shrine rites (shaso, betto) under honji-suijaku framing, collecting revenue and social authority from both traditions simultaneously. Doctrinal reconciliation was never institutionally required of them; the absence of enforcement let this dual role persist for centuries and generated income neither tradition alone would have supported.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, buddhist_clergy_managing_shrine_precincts, beneficiary,
    organized, generational, arbitrage, regional).

% Shrine priests whose institutions depended on Buddhist temple patronage and combinatory ritual calendars for funding and legitimacy. They gained materially from the arrangement but also had no institutional path to resolve which cosmology their own rites actually presupposed, leaving their own doctrinal position permanently unsettled.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, local_kami_priests_retaining_temple_patronage, beneficiary,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, local_kami_priests_retaining_temple_patronage, payer).

% Ordinary worshippers who venerated kami for this-worldly matters and buddhas for salvation without any authoritative account of how these commitments related to one another. They bore the cost of unexamined contradiction — performing rites whose combined logic no institution would explain to them — and had no local venue to raise the question without appearing impious toward both.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, lay_practitioners_seeking_doctrinal_coherence, payer,
    powerless, biographical, trapped, local).

% National-learning scholars who argued the combinatory system obscured a purer, indigenous kami tradition beneath centuries of Buddhist accretion. They pressed for doctrinal clarity and paid a social and professional cost for raising the incoherence question before it was politically convenient, marginalized by institutions that profited from leaving it unresolved.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, reform_minded_kokugaku_scholars, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, reform_minded_kokugaku_scholars, excluded).

% Communities obligated to fund and staff both shrine festivals and temple observances year-round, absorbing the labor and material cost of maintaining two overlapping ritual calendars whose relationship to each other was never authoritatively settled, with no mechanism to consolidate or question the double burden.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, village_communities_bearing_dual_ritual_obligations, payer,
    powerless, generational, trapped, local).

% State officials who issued the 1868 shinbutsu-bunri edicts, treating the prior arrangement not as a stable synthesis to be dismantled but as an unresolved contradiction finally forced into visibility, using the moment to construct a purified State Shinto that served new nationalist and administrative purposes.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, meiji_state_shinto_architects, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, meiji_state_shinto_architects, beneficiary).

% Scholars who examine temple-shrine records, doctrinal treatises, and lay testimony to assess whether pre-Meiji practitioners held a coherent synthesis, a functional partition, or an unexamined contradiction sustained by institutional convenience rather than resolved belief.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, comparative_religion_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The joint jingu-ji institutions did solve a real administrative problem — consolidating land, labor, and ritual calendars across two religious traditions under one local authority reduced duplication of institutional overhead in premodern Japan.
% TRANSFER_FUNCTION: The arrangement moved unexamined cognitive and ritual-labor costs from the administering institutions (which captured combined revenue and authority) onto lay practitioners and village communities, who bore the burden of sustaining two ritual obligations and an unresolved metaphysical picture with no institutional accounting of the contradiction.
% ABSENT_VOICES: Ordinary practitioners who might have asked how kami and buddha veneration related to each other had no doctrinal forum in which to raise the question without seeming to reject one tradition; kokugaku scholars who did raise it were treated as fringe until the Meiji state found the argument politically useful. Neither voice shaped the arrangement while it stood.
% DISAPPEARANCE_RATIONALE: When the Meiji government forcibly separated kami and buddha worship in 1868, the combined jingu-ji institutions were dissolved, land and clergy were reassigned, ritual calendars were split, and centuries of administrative consolidation unwound within a few years — evidence that the prior arrangement had been actively institutionally load-bearing, not a free-floating belief system that could vanish without effect.
% FOUNDING_PROBLEM: Early religious administrators needed to integrate an indigenous kami cultic system with an incoming, more doctrinally elaborated Buddhist tradition without triggering the kind of sectarian conflict that doctrinal reconciliation efforts elsewhere had produced; combinatory practice and honji-suijaku theory offered official-sounding cover for not resolving the underlying question.
% FOUNDING_PROBLEM_CORROBORATION: Kokugaku scholars (outside the beneficiary institutions) attested as early as the 18th century that the combinatory framework was an unprincipled accretion rather than a solved theological problem; Meiji state investigators reached the same conclusion when cataloguing shrine-temple complexes for separation, finding no consistent doctrinal account practitioners could produce when asked directly. No comparably-placed voice from within the beneficiary institutions ever offered an alternative corroboration of a resolved synthesis.
narrative_ontology:disappearance_verdict(simultaneous_veneration__pragmatic_incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__pragmatic_incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__pragmatic_incoherence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(simultaneous_veneration__pragmatic_incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__pragmatic_incoherence_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71 at interval end) because, on this reading, the arrangement's apparent stability was purchased by never subjecting the combinatory cosmology to the kind of doctrinal accounting that would have exposed its contradictions — a cost continuously transferred to lay practitioners and village communities who could not question it without social risk. Theater ratio rises across the interval (0.35 to 0.62) because as centuries passed, honji-suijaku theorizing increasingly functioned as elaborate justificatory performance for an arrangement whose primary function had become institutional revenue consolidation rather than genuine theological integration. Suppression is authored as moderate rather than extreme (0.58) because the mechanism was less coercive enforcement than structural absence of any venue for doctrinal challenge — practitioners were not punished for raising the question so much as there was no institution positioned to hear it, until kokugaku scholarship and later Meiji policy created that venue.
 *
 * DIRECTIONALITY LOGIC:
 *   The jingu-ji administrative complexes and the buddhist clergy who ran dual shrine-temple roles are structural beneficiaries: they collected the combined revenue and authority the ambiguity made possible, so their derived directionality sits near the beneficiary end. Lay practitioners and village communities are structural targets: trapped exit options (no alternative ritual infrastructure existed at the local level), and they bore the accumulated cost of an arrangement never doctrinally reconciled on their behalf. Kokugaku scholars occupy an unusual position — payers in social and professional terms for raising the incoherence question, but also partially excluded from the institutional conversation until their critique became useful to a later agenda-setter (the Meiji state), at which point they shifted from dissenting outsider to retrospectively vindicated voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — integrating two religious traditions without triggering sectarian conflict — is authored as dead by the time of Meiji separation: the practical need for cover-story ambiguity had long since been superseded by institutional inertia and revenue capture. The classification prevents mislabeling this as pure coordination (which would erase the suppressed-contradiction reading's central claim) while also refusing to treat it as a pure snare, since the administrative jingu-ji complexes did perform some genuine consolidation function historically — hence tangled_rope rather than snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incoherence_vs_synthesis_evidentiary_basis,
    'Is the historical record better explained by genuine unreconciled contradiction among practitioners (this reading), by a coherent but non-Western metaphysical fusion (ontological_fusion_reading), or by functional domain specialization that required no metaphysical reconciliation at all (domain_partition_reading)?',
    'Systematic analysis of pre-Meiji doctrinal treatises, temple administrative records, and lay confraternity documents for evidence of practitioners either (a) articulating a coherent synthetic account, (b) treating the traditions as domain-separated without felt tension, or (c) exhibiting the kind of unexamined, institutionally-shielded contradiction this reading claims.',
    'If treatises show consistent articulated synthesis, this reading''s high-ε claim is undermined in favor of ontological_fusion_reading; if lay practice shows untroubled domain specialization with no suppressed tension, domain_partition_reading is favored instead. Only genuine evidence of institutionally-shielded, practitioner-level unresolved contradiction supports this reading''s classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incoherence_vs_synthesis_evidentiary_basis, empirical, 'Whether the primary historical record supports incoherence over fusion or partition.').

omega_variable(
    meiji_rupture_vs_revelation,
    'Was the 1868 shinbutsu-bunri edict an artificial state-imposed rupture of a previously functioning arrangement, or the forced revelation of a contradiction that institutions had long suppressed?',
    'Compare the speed and apparent ease of institutional separation (jingu-ji dissolution records, clergy reassignment patterns, local resistance or lack thereof) against what would be expected if the prior arrangement had been deeply doctrinally load-bearing versus administratively convenient but doctrinally hollow.',
    'Rapid, low-resistance separation with minimal doctrinal defense from within institutions supports this reading''s revelation account; sustained doctrinal defense or popular resistance grounded in genuine synthetic belief would favor a rupture account and undercut the high-ε claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_rupture_vs_revelation, conceptual, 'Whether Meiji separation reveals latent incoherence or imposes an external rupture.').

omega_variable(
    kernel_framing_underdetermination,
    'Could the same combinatory practice record be equally well described under the domain_partition_reading''s framing (specialization, not incoherence) with only interpretive emphasis differing, rather than a genuinely distinct structural claim?',
    'Test whether practitioners who could articulate a domain-partition rationale when asked (this-worldly kami rites vs. salvific buddha rites) would count as evidence against ''unresolved contradiction'' — i.e., whether inarticulacy about metaphysical unity is different in kind from inarticulacy about functional partition.',
    'If most practitioners could readily state a partition rationale on inquiry, this reading''s core claim (that no resolution existed, even a functional one) weakens substantially and the constraint''s ε should be revisited downward toward the domain_partition_reading''s presumably lower extraction profile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether pragmatic incoherence and domain partition are genuinely distinct claims or differ mainly in interpretive framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__pragmatic_incoherence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(simu_tr_t20, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(simu_tr_t40, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement(simu_tr_t60, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 60, 0.53).
narrative_ontology:measurement(simu_tr_t80, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 80, 0.58).
narrative_ontology:measurement(simu_tr_t100, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 100, 0.62).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(simu_be_t20, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(simu_be_t40, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(simu_be_t60, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 60, 0.64).
narrative_ontology:measurement(simu_be_t80, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement(simu_be_t100, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 100, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t0, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(simu_su_t20, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(simu_su_t40, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(simu_su_t60, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 60, 0.46).
narrative_ontology:measurement(simu_su_t80, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 80, 0.52).
narrative_ontology:measurement(simu_su_t100, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__pragmatic_incoherence_reading, identity_coordination).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration__ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration__domain_partition_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the simultaneous_veneration kernel (shinbutsu-shugo). pragmatic_incoherence_reading authors high ε (0.71) on the claim that the arrangement was suppressed contradiction rather than resolved doctrine; ontological_fusion_reading and domain_partition_reading are expected to author substantially lower ε, since each holds the arrangement was either metaphysically coherent or functionally well-specified rather than unresolved. All three share the same historical kernel (institutionalized kami-buddha co-veneration in premodern Japan) but diverge on beneficiary/victim structure and on whether Meiji separation constitutes rupture or revelation — hence three distinct constraints rather than one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
