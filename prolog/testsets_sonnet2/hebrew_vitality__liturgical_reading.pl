% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__liturgical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__liturgical_reading, []).

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
 *   constraint_id: hebrew_vitality__liturgical_reading
 *   human_readable: Liturgical Continuity as Constitutive Vitality (Hebrew, Kernel Reading)
 *   domain: sociolinguistics/religious_studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested 'Hebrew vitality'
 *   kernel: the claim that unbroken liturgical use itself constitutes
 *   linguistic vitality, without requiring vernacular native-speaker
 *   transmission. On this reading, the diaspora-era practice of maintaining
 *   Hebrew as a stable recitation and cantillation register across centuries
 *   and continents is not merely preservation of a dead language for ritual
 *   purposes — it IS the language's vitality, full stop. This is structurally
 *   distinct from the sibling readings (native_daily_reading: only spoken
 *   native transmission counts; hybrid_continuity_reading: liturgical
 *   preservation was a necessary substrate but insufficient alone). Per the
 *   ε-invariance principle, these are three different constraints sharing a
 *   kernel, not one constraint measured three ways — this file authors only
 *   the liturgical reading, with its own ε, its own beneficiary set, and no
 *   victim set, because on this reading's own terms preservation imposes no
 *   cost on anyone: it does not compete with or suppress vernacular efforts,
 *   it simply constitutes a separate, self-sufficient criterion of vitality.
 *
 * KEY AGENTS:
 *   - rabbinic_authorities: institutional agenda-setter and beneficiary — administers and certifies liturgical correctness, standing constituted by continuity itself
 *   - liturgical_communities: organized beneficiary — receive communal identity and continuity without needing vernacular Hebrew
 *   - liturgical_readers: moderate-power beneficiary/payer — invest years mastering ritual-register competence
 *   - vernacular_hebrew_advocates: excluded — hold the sibling reading, not consulted inside this kernel reading
 *   - comparative_linguists: analytical observer — evaluates the reading's internal coherence against general vitality theory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__liturgical_reading, 0.18).
domain_priors:suppression_score(hebrew_vitality__liturgical_reading, 0.22).
domain_priors:theater_ratio(hebrew_vitality__liturgical_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__liturgical_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__liturgical_reading, "Liturgical Continuity as Constitutive Vitality (Hebrew, Kernel Reading)").
narrative_ontology:topic_domain(hebrew_vitality__liturgical_reading, "sociolinguistics/religious_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__liturgical_reading, '53cce068-b733-4bee-840e-211614357cb0').
narrative_ontology:cs_kernel_codification('53cce068-b733-4bee-840e-211614357cb0', distributed).
narrative_ontology:cs_authority_grounding('53cce068-b733-4bee-840e-211614357cb0', lineage).
narrative_ontology:cs_interpretation_layer_present('53cce068-b733-4bee-840e-211614357cb0').
narrative_ontology:cs_reading_relation('53cce068-b733-4bee-840e-211614357cb0', hebrew_vitality__native_daily_reading, forecloses).
narrative_ontology:cs_reading_relation('53cce068-b733-4bee-840e-211614357cb0', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('53cce068-b733-4bee-840e-211614357cb0', foundational, ritual_continuity_is_sufficient_for_vitality).
narrative_ontology:cs_axiom_status(ritual_continuity_is_sufficient_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('53cce068-b733-4bee-840e-211614357cb0', ritual_continuity_is_sufficient_for_vitality, conventional).
narrative_ontology:cs_axiom('53cce068-b733-4bee-840e-211614357cb0', secondary, vernacular_native_transmission_not_required_for_life).
narrative_ontology:cs_axiom_status(vernacular_native_transmission_not_required_for_life, holdable).
narrative_ontology:cs_axiom_grounding('53cce068-b733-4bee-840e-211614357cb0', vernacular_native_transmission_not_required_for_life, conventional).
narrative_ontology:cs_reference_frame('53cce068-b733-4bee-840e-211614357cb0', diaspora_unbroken_recitation_tradition).
narrative_ontology:cs_drift_state('53cce068-b733-4bee-840e-211614357cb0', post_zionist_revival_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('53cce068-b733-4bee-840e-211614357cb0', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__liturgical_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, liturgical_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, liturgical_readers).
narrative_ontology:constraint_victim(hebrew_vitality__liturgical_reading, liturgical_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer synagogue liturgy, determine correct recitation, train readers, and certify what counts as proper continuation of the textual tradition. Their authority is constituted by being the custodians of continuous ritual use; the practice of unbroken liturgical reading is the institutional basis of their standing across diaspora communities.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, rabbinic_authorities, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__liturgical_reading, rabbinic_authorities, beneficiary).

% Congregations and prayer communities that maintain Hebrew liturgical recitation as an unbroken practice across centuries and geographies. They receive continuity of communal identity, ritual meaning, and connective thread to prior generations through the maintained practice itself, without needing Hebrew as a spoken vernacular.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, liturgical_communities, beneficiary,
    organized, generational, constrained, global).

% Individuals who learn to chant and recite liturgical Hebrew, investing years in mastering cantillation, pronunciation traditions, and textual competence limited to ritual registers. They gain communal status and religious meaning from this competence; the cost is time invested in a register that does not transfer to conversational fluency.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, liturgical_readers, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__liturgical_reading, liturgical_readers, payer).

% Advocates and institutions oriented toward spoken, native-generation Hebrew (the sibling reading) who are not part of the liturgical authority structure. They would argue that ritual-only continuity does not constitute linguistic vitality, but their claim is not adjudicated inside this constraint's own kernel — the liturgical reading treats their standard as a different question entirely.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, vernacular_hebrew_advocates, excluded,
    organized, generational, mobile, national).

% Scholars of language death and revitalization who evaluate whether ritual-register-only survival meets standard vitality criteria (intergenerational native transmission, domain expansion). They document the liturgical reading's internal coherence without adjudicating between the kernel's competing readings.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, comparative_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:fixing_cost_class(hebrew_vitality__liturgical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a dispersed, multi-century, multi-continental community around a single stable liturgical text and recitation practice, allowing communities separated by geography and vernacular language to share a common ritual register and mutual intelligibility in prayer.
% TRANSFER_FUNCTION: Moves interpretive and certifying authority over 'what counts as living Hebrew' to those who administer unbroken liturgical practice; moves time and training investment from individual readers into ritual competence rather than conversational competence, without any correspondingly extracted material transfer.
% ABSENT_VOICES: Advocates of vernacular/native-generation Hebrew (the native_daily_reading) and hybrid-continuity scholars are structurally outside this reading's kernel — this reading does not consult them because its own criterion (unbroken use) does not require vernacular attestation to be satisfied.
% DISAPPEARANCE_RATIONALE: If liturgical continuity vanished, rabbinic authorities and liturgical communities would lose a central organizing practice and identity marker — the world clearly rearranges for them. But comparative linguists and vernacular-Hebrew advocates would say nothing changes for 'vitality' in the standard sense, since on their account the liturgical practice was never constitutive of vitality to begin with. The verdict differs depending on which reading of the kernel is applied, which is exactly the structural fact this story exists to isolate.
% FOUNDING_PROBLEM: Diaspora dispersion threatened to fragment Hebrew textual and liturgical practice entirely; unbroken ritual recitation was instituted/maintained to prevent total discontinuity of the textual tradition across communities lacking a shared vernacular.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities and liturgical communities (the benefiting parties) attest the founding problem remains live and that liturgical continuity itself satisfies it. Outside corroboration is mixed: historical linguists documenting diaspora Hebrew confirm the discontinuity-prevention function was real and effective for textual/ritual survival, but do not corroborate that this alone constitutes 'vitality' in the sense used by revitalization scholarship — that further claim is contested by voices outside the liturgical tradition.
narrative_ontology:disappearance_verdict(hebrew_vitality__liturgical_reading, contested).
narrative_ontology:founding_problem_status(hebrew_vitality__liturgical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__liturgical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_vitality__liturgical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__liturgical_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__liturgical_reading_tests).
:- end_tests(hebrew_vitality__liturgical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the liturgical reading, on its own terms, imposes no rent-extraction: readers who invest in ritual competence receive communal status and religious meaning in direct exchange, and no party is structurally worse off for the practice's continuation. Suppression (0.22) reflects the real but modest social pressure toward correct ritual conformity (fixed pronunciation traditions, cantillation norms) rather than any coercive exclusion of alternatives — communities that lapse in liturgical practice are not punished, merely drift from the tradition. Accessibility collapse (0.35) is moderate: alternative ways of maintaining Jewish communal identity exist and are not foreclosed by this reading, but within observant communities the liturgical register is treated as the default, unquestioned form of 'Hebrew still being alive.' Resistance (0.3) is likewise moderate — mostly friction from vernacular-Hebrew advocates and secular Zionist revivalist history, not resistance from within the liturgical communities themselves.
 *
 * PERSPECTIVAL GAP:
 *   From inside the liturgical tradition (rabbinic authorities, liturgical communities), the practice fully satisfies the criterion of vitality and no gap exists between claim and lived experience. From the analytical/comparative-linguistics seat, the same practice reads as a coherent but narrow criterion that differs sharply from the standard sociolinguistic definition of vitality (which requires intergenerational native transmission and unrestricted domain use) — this is not a factual disagreement about the practice, but a disagreement about which practice counts as satisfying the word 'vitality.' The engine should compute both seats' classifications from the same structural data; the gap is expected and is the point of decomposing the kernel into separate readings rather than averaging them.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities sit closest to the beneficiary end: their institutional authority is partly constituted by the continuity practice they administer, so the constraint subsidizes their standing directly. Liturgical communities and readers are also beneficiaries — they receive the goods (communal continuity, ritual meaning) that the practice exists to produce, and their 'cost' (time invested in liturgical-only competence) is a chosen investment matched by a corresponding good, not extraction. There is no victim set on this reading: no party bears cost without corresponding benefit, because the reading's own criterion for success (unbroken use) is satisfied by the very practice that benefits its participants. Vernacular Hebrew advocates are excluded rather than victimized — this reading does not act on them at all, positive or negative.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading has no plausible mandatrophy claim in isolation: the founding problem (diaspora fragmentation threatening total discontinuity of Hebrew textual tradition) is still corroborated as live by observant communities, and the practice that solves it is the same practice being evaluated. Mandatrophy risk would only arise if the liturgical-only criterion were used to actively suppress or defund vernacular revitalization efforts elsewhere — but this reading, authored cleanly, makes no such extractive move; it is a self-contained criterion with a self-contained beneficiary set and no victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_criterion_definitional_dispute,
    'Is ''vitality'' properly a property of a spoken vernacular (requiring native intergenerational transmission), or can it be constituted by unbroken formal/ritual use across a textual tradition without vernacular continuity?',
    'No empirical resolution is possible — this is a definitional/conceptual dispute about what the word ''vitality'' picks out in sociolinguistics vs. in religious-communal self-understanding. Resolution would require either a stipulated technical definition accepted across both fields, or an acknowledgment that the term is doing different work in each domain.',
    'If the native-speaker criterion is adopted as the sole valid definition, this reading''s central claim collapses and the constraint would need to be reclassified as describing ''preservation'' rather than ''vitality'' — closer to the native_daily_reading''s own framing of what liturgical practice actually is. If the liturgical criterion is accepted as co-valid, this reading stands independently.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vitality_criterion_definitional_dispute, conceptual, 'Whether liturgical continuity alone can satisfy the concept of linguistic vitality, or only vernacular transmission can.').

omega_variable(
    rabbinic_authority_beneficiary_ambiguity,
    'Does rabbinic authorities'' status as beneficiaries reflect a genuine coordination function (maintaining shared liturgical intelligibility across diaspora) or does it partly reflect institutional self-interest in defining vitality in a way that centers their own certifying role?',
    'Historical comparison across periods/communities where rabbinic authority over liturgical correctness was weaker or contested (e.g., competing nusach traditions, Reform-era liturgical reform movements) to see whether the coordination function persisted independent of any particular authority structure''s institutional interest.',
    'If authority over liturgical definition is separable from the coordination function itself, the low extractiveness score is robust. If the two are entangled, some portion of the apparently non-extractive beneficiary relationship may reflect ordinary institutional self-perpetuation rather than pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rabbinic_authority_beneficiary_ambiguity, conceptual, 'Whether rabbinic beneficiary status reflects pure coordination or partial institutional self-interest in the vitality claim.').

omega_variable(
    framing_choice_kernel_vs_authority,
    'Should this story''s kernel be framed as the liturgical PRACTICE itself (unbroken recitation), or as the LEGITIMACY CLAIM layered above the practice (that this practice is sufficient to constitute vitality, as opposed to merely constituting preservation)?',
    'Compare classification outcomes under both framings: under the practice-framing, this is a stable cultural-linguistic coordination mechanism (rope-like); under the legitimacy-claim-framing, the story is really about an interpretive/definitional authority claim layered over the practice, closer to a commitment-system kernel dispute (as modeled here via cs_structure).',
    'The chosen framing (legitimacy-claim-over-practice) is what licenses the cs_structure/reading_relations apparatus used in this file. Had the practice-only framing been chosen, this story would have no meaningful kernel contest to model and would likely have been authored as an ordinary rope without cs_structure fields.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_choice_kernel_vs_authority, conceptual, 'Whether the kernel is the liturgical practice or the legitimacy claim that the practice constitutes vitality — this file adopts the latter framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__liturgical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__liturgical_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hebr_tr_t20, hebrew_vitality__liturgical_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(hebr_tr_t40, hebrew_vitality__liturgical_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(hebr_tr_t60, hebrew_vitality__liturgical_reading, theater_ratio, 60, 0.13).
narrative_ontology:measurement(hebr_tr_t80, hebrew_vitality__liturgical_reading, theater_ratio, 80, 0.14).
narrative_ontology:measurement(hebr_tr_t100, hebrew_vitality__liturgical_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__liturgical_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(hebr_be_t20, hebrew_vitality__liturgical_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(hebr_be_t40, hebrew_vitality__liturgical_reading, base_extractiveness, 40, 0.16).
narrative_ontology:measurement(hebr_be_t60, hebrew_vitality__liturgical_reading, base_extractiveness, 60, 0.17).
narrative_ontology:measurement(hebr_be_t80, hebrew_vitality__liturgical_reading, base_extractiveness, 80, 0.17).
narrative_ontology:measurement(hebr_be_t100, hebrew_vitality__liturgical_reading, base_extractiveness, 100, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_vitality__liturgical_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__liturgical_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__native_daily_reading).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language concept 'Hebrew language vitality across the diaspora and into revival.' The three readings share a kernel (what criterion determines whether Hebrew was/is 'alive') but diverge structurally: liturgical_reading (this file) claims ritual continuity alone suffices, ε low, beneficiary = rabbinic authorities, no victims; native_daily_reading claims only vernacular native transmission counts, treating ritual recitation as mere preservation; hybrid_continuity_reading claims liturgical preservation was necessary substrate but insufficient alone, requiring subsequent vernacular reconstruction (the historical Ben-Yehuda-era revival). Per the ε-invariance principle these are linked via network.affects_constraints rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
