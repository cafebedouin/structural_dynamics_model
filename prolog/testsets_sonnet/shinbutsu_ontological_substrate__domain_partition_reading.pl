% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__domain_partition_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__domain_partition_reading
 *   human_readable: Shinbutsu Domain-Partition Reading: Kami/Buddha Functional Coexistence
 *   domain: religious_studies/institutional_history
 *
 * SUMMARY:
 *   This story instantiates the domain_partition_reading of the
 *   shinbutsu_ontological_substrate kernel: the claim that kami cult and
 *   Buddhist institutions in premodern Japan coexisted because they answered
 *   structurally non-overlapping needs (this-worldly welfare versus
 *   afterlife/salvation), not because their metaphysics was unified. On this
 *   reading, honji suijaku theorizing is a later, separable interpretive
 *   overlay rather than a description of what made coexistence possible — the
 *   coexistence itself is read as low-friction functional division of ritual
 *   labor. This is deliberately NOT the syncretic_fusion_reading (which
 *   treats honji suijaku as revealing metaphysical truth about a single
 *   underlying reality) and NOT the incoherent_bundle_reading (which denies
 *   any coherent kernel and treats the whole arrangement as accumulated
 *   state-enforced drift). Each of the three readings is authored as its own
 *   constraint story with its own epsilon; this file's epsilon is low because
 *   the partition reading, by its own premises, requires minimal enforcement
 *   — the domains simply do not compete.
 *
 * KEY AGENTS:
 *   - shrine_priesthoods: agenda_setter/beneficiary (organized/regional) — kami ritual jurisdiction, this-world domain
 *   - temple_clergy: agenda_setter/beneficiary (organized/regional) — Buddhist ritual jurisdiction, afterlife domain
 *   - local_communities: beneficiary (moderate/local) — patronize both without doctrinal conflict
 *   - doctrinal_systematizers: excluded (moderate/national) — honji suijaku theorists sidelined by the partition account
 *   - state_authorities: observer/beneficiary (institutional/national) — administrative convenience of separable institutions, later formalized in Meiji separation edicts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__domain_partition_reading, 0.28).
domain_priors:suppression_score(shinbutsu_ontological_substrate__domain_partition_reading, 0.22).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__domain_partition_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__domain_partition_reading, "Shinbutsu Domain-Partition Reading: Kami/Buddha Functional Coexistence").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__domain_partition_reading, "religious_studies/institutional_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__domain_partition_reading, '5800c7cc-643c-41f3-bd5a-2a37456425f7').
narrative_ontology:cs_kernel_codification('5800c7cc-643c-41f3-bd5a-2a37456425f7', distributed).
narrative_ontology:cs_authority_grounding('5800c7cc-643c-41f3-bd5a-2a37456425f7', practice).
narrative_ontology:cs_interpretation_layer_present('5800c7cc-643c-41f3-bd5a-2a37456425f7').
narrative_ontology:cs_reading_relation('5800c7cc-643c-41f3-bd5a-2a37456425f7', shinbutsu_ontological_substrate__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('5800c7cc-643c-41f3-bd5a-2a37456425f7', shinbutsu_ontological_substrate__incoherent_bundle_reading, influences).
narrative_ontology:cs_axiom('5800c7cc-643c-41f3-bd5a-2a37456425f7', foundational, domains_are_functionally_nonoverlapping).
narrative_ontology:cs_axiom_status(domains_are_functionally_nonoverlapping, holdable).
narrative_ontology:cs_axiom_grounding('5800c7cc-643c-41f3-bd5a-2a37456425f7', domains_are_functionally_nonoverlapping, conventional).
narrative_ontology:cs_axiom('5800c7cc-643c-41f3-bd5a-2a37456425f7', foundational, coexistence_requires_no_ontological_reconciliation).
narrative_ontology:cs_axiom_status(coexistence_requires_no_ontological_reconciliation, holdable).
narrative_ontology:cs_axiom_grounding('5800c7cc-643c-41f3-bd5a-2a37456425f7', coexistence_requires_no_ontological_reconciliation, instrumental).
narrative_ontology:cs_reference_frame('5800c7cc-643c-41f3-bd5a-2a37456425f7', domain_bounded_ritual_specialization).
narrative_ontology:cs_drift_state('5800c7cc-643c-41f3-bd5a-2a37456425f7', meiji_shinbutsu_bunri_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5800c7cc-643c-41f3-bd5a-2a37456425f7', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, shrine_priesthoods).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, temple_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, local_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, state_authorities).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__domain_partition_reading, domain_partition_coherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers kami ritual for this-worldly concerns — harvest, purification, community protection, life-stage rites. Under the domain-partition reading, this jurisdiction is uncontested by Buddhist institutions because the domains genuinely do not overlap; the priesthood collects offerings and social standing for functions Buddhist temples do not claim to perform.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, shrine_priesthoods, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__domain_partition_reading, shrine_priesthoods, beneficiary).

% Administers Buddhist ritual for afterlife and salvific concerns — funerary rites, ancestor memorial, karmic liberation. On this reading their authority is functionally bounded to soteriology and does not require subordinating or absorbing kami cult, since the two systems answer different questions.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, temple_clergy, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__domain_partition_reading, temple_clergy, beneficiary).

% Patronize both shrine and temple as a matter of practical life-need allocation: kami for harvest and birth, buddhas for death and memorial. Under the partition reading they experience no doctrinal tension because they are not being asked to hold one metaphysics — they are using two service providers for two different problems.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, local_communities, beneficiary,
    moderate, biographical, constrained, local).

% Honji suijaku theorists and medieval commentators who argued for a unified metaphysical account (kami as local manifestations of buddhas) are sidelined by this reading, which treats their systematizing project as institutional overlay rather than as revealing an underlying ontological truth. Their voice would object that partition flattens a real metaphysical claim into mere administrative convenience.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, doctrinal_systematizers, excluded,
    moderate, generational, constrained, national).

% Historically found the coexistence administratively convenient — two functioning institutions covering different social needs without requiring doctrinal adjudication. Later Meiji-era separation edicts (shinbutsu bunri) presupposed exactly this partition logic, treating kami and buddha institutions as always-already separable.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, state_authorities, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__domain_partition_reading, state_authorities, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divides labor between two ritual specialist systems along a genuine functional seam — this-worldly welfare versus afterlife/salvation — so that neither institution needs to claim jurisdiction over the other's domain, and communities can access both without contradiction.
% TRANSFER_FUNCTION: Moves ritual patronage, land grants, and social standing to shrine and temple institutions respectively, each collecting within its own functional lane; no systematic transfer runs from one institution to the other under this reading, since the domains do not compete for the same resource base.
% ABSENT_VOICES: Honji suijaku systematizers and later State Shinto ideologues who insisted on either metaphysical fusion or a hierarchical ranking (kami as buddha-emanations or vice versa) are structurally excluded from this reading's account — the partition reading treats their metaphysical claims as unnecessary superstructure on a coexistence that was functional all along.
% DISAPPEARANCE_RATIONALE: If the domain-partition understanding vanished, shrine and temple institutions would still materially persist (they have separate land bases, separate lineages, separate patronage), but the ideological account of WHY they coexist without conflict would need replacing — either by the fusion account or by an admission of pure historical accretion. Whether the world 'rearranges' is exactly what is contested between the three kernel readings.
% FOUNDING_PROBLEM: Pre-modern Japanese communities faced two distinct classes of need — this-worldly welfare (harvest, health, protection) and afterlife/salvific concern (death, karma, ancestor status) — for which indigenous kami cult and imported Buddhist doctrine offered non-competing answers; the partition reading holds that formalizing this division let both systems operate without forcing a doctrinal winner.
% FOUNDING_PROBLEM_CORROBORATION: Meiji-era shinbutsu bunri administrators, writing from OUTSIDE the medieval clergy who benefited from ambiguity, treated kami and buddha institutions as separable as a matter of administrative fact, which is read by some historians as corroborating that a functional partition was real and pre-existing rather than invented at separation. Countervailing corroboration is weak: no medieval documentary source describes the arrangement in partition terms — the language of the period is overwhelmingly honji suijaku (fusion) language, which is itself evidence for the rival reading and is noted here rather than suppressed.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__domain_partition_reading, contested).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__domain_partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__domain_partition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).
:- end_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28 at interval end) because, on the partition reading's own terms, neither institution is extracting from the other's domain — each collects patronage within its own functional lane. Suppression is low (0.22) because no active enforcement is required to keep the domains separate if they genuinely do not overlap; theater_ratio rises modestly over the interval (0.15 to 0.34) reflecting later performative harmonization rhetoric (state-sponsored syncretism narratives) layered atop what the reading holds was originally a plain division of labor. Accessibility_collapse is moderate (0.35): communities could in principle have chosen a single-system solution (pure kami cult or pure Buddhist cosmology) but did not, which is read as revealing genuine parallel utility rather than monopolistic lock-in.
 *
 * PERSPECTIVAL GAP:
 *   From inside the shrine or temple institutional seat, the partition reading is simply an accurate description of what their institution does — no felt tension, no need for metaphysical reconciliation. From the systematizer's seat (excluded here), the partition reading looks like it flattens a genuine, historically documented metaphysical claim (kami as buddha-manifestations) into bureaucratic convenience after the fact. The engine computes these as structurally different positions; this file does not adjudicate which seat is correct — it only authors the domain-partition seat cleanly.
 *
 * DIRECTIONALITY LOGIC:
 *   Shrine and temple institutions are both near-symmetric beneficiaries under this reading — each holds an uncontested functional monopoly within its lane, so neither directionality is strongly extractive toward the other. Local communities sit close to true beneficiary status: they get two non-competing service systems for two distinct classes of need, at ordinary patronage cost. The doctrinal_systematizers occupy the excluded seat because their metaphysical unification project is treated by this reading as unnecessary and is structurally written out of the account, not because they bear material cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for parallel non-competing ritual systems) is marked contested rather than resolved-dead, because whether the functional division was ever the operative logic, versus a retrospective simplification imposed by later administrators (notably Meiji separation reformers), is exactly the disputed genealogical question the omega below tracks. This prevents the story from either over-claiming mandatrophy resolution or presuming the partition account is uncontested historical fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_retrospective_or_original,
    'Was the this-world/afterlife domain division an operative principle held by medieval practitioners, or is it a modern scholarly and Meiji-administrative retrofit imposed on a messier, more genuinely fused historical practice?',
    'Close reading of medieval ritual manuals, temple-shrine complex (jingu-ji) administrative records, and votive inscriptions for evidence of explicit domain-based reasoning versus fusion-based reasoning in practitioners'' own terms, cross-checked against Meiji-era shinbutsu bunri edict language for anachronistic backprojection.',
    'If practitioners'' own sources show domain reasoning, the partition reading gains standing as a genealogically real coordination logic; if sources show only fusion language, the partition reading is better classified as a modern reconstruction and its low extractiveness score would need re-examination as itself a retrospective artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_retrospective_or_original, empirical, 'Whether domain partition is a period-authentic operative logic or a later retrofit.').

omega_variable(
    shinbutsu_kernel_reading_selection,
    'Is the domain-partition reading, the syncretic-fusion reading, or the incoherent-bundle reading the structurally correct account of shinbutsu shugo — or do all three coexist as genuinely different parties'' commitments with no fact of the matter adjudicating between them?',
    'This is the committer-frame question routed here per Rule 2: comparative analysis across the three linked constraint stories (domain_partition, syncretic_fusion, incoherent_bundle), examining which reading each historical faction (shrine priesthoods, temple clergy, honji suijaku theorists, Meiji state reformers) actually held and whether any single reading commanded cross-factional assent at any point.',
    'If one reading commanded broad cross-factional assent historically, it should be weighted as the dominant structural account; if all three were held by different factions simultaneously with no resolution, the kernel itself is best modeled as permanently contested rather than resolvable, and none of the three sibling stories should be treated as the ''true'' one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(shinbutsu_kernel_reading_selection, conceptual, 'Which kernel reading (or none) is structurally authoritative across the shinbutsu_ontological_substrate contest.').

omega_variable(
    systematizer_exclusion_cost,
    'Does excluding honji suijaku systematizers from this reading''s account impose any real cost on them, or is their exclusion purely representational (a matter of whose theory is centered in the historical narrative) with no material stakes?',
    'Examine whether systematizer lineages (e.g. Tendai/Shingon syncretist schools) suffered material institutional consequences when domain-partition-style separation was later state-enforced (Meiji shinbutsu bunri), which would convert a representational exclusion into a material one.',
    'If Meiji-era separation materially harmed syncretist institutions by delegitimizing their fusion doctrine, the domain-partition reading''s modern institutional legacy would need to be read as partly extractive toward that specific victim class, which is not currently reflected in this story''s victims array (none declared).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systematizer_exclusion_cost, empirical, 'Whether excluding fusion-theorists from this reading has downstream material consequences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__domain_partition_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(shin_tr_t0, projected).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 200, 0.18).
narrative_ontology:measurement_basis(shin_tr_t200, projected).
narrative_ontology:measurement(shin_tr_t400, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 400, 0.22).
narrative_ontology:measurement_basis(shin_tr_t400, projected).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 600, 0.26).
narrative_ontology:measurement_basis(shin_tr_t600, projected).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 900, 0.3).
narrative_ontology:measurement_basis(shin_tr_t900, projected).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1200, 0.34).
narrative_ontology:measurement_basis(shin_tr_t1200, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(shin_be_t0, projected).
narrative_ontology:measurement(shin_be_t200, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 200, 0.2).
narrative_ontology:measurement_basis(shin_be_t200, projected).
narrative_ontology:measurement(shin_be_t400, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 400, 0.22).
narrative_ontology:measurement_basis(shin_be_t400, projected).
narrative_ontology:measurement(shin_be_t600, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 600, 0.24).
narrative_ontology:measurement_basis(shin_be_t600, projected).
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 900, 0.26).
narrative_ontology:measurement_basis(shin_be_t900, projected).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1200, 0.28).
narrative_ontology:measurement_basis(shin_be_t1200, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shinbutsu_ontological_substrate__domain_partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_substrate__domain_partition_reading, 0.1).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% Three-member constraint family under the shinbutsu_ontological_substrate kernel. domain_partition_reading (this story) asserts functional but non-ontological coexistence with low institutional entanglement; syncretic_fusion_reading asserts genuine metaphysical unification (honji suijaku as ontological truth); incoherent_bundle_reading denies any coherent kernel and treats the appearance of system as state-enforced institutional accretion (particularly via Edo-period danka/terauke administration). Each story carries its own epsilon and stakeholder structure per the ε-invariance principle; they are linked here rather than merged because measuring 'shinbutsu shugo' by institutional-separability criteria versus doctrinal-content criteria versus enforcement-history criteria yields three different extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
