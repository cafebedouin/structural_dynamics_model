% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deletionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__deletionist_reading, []).

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
 *   constraint_id: notability_guidelines__deletionist_reading
 *   human_readable: WP:N Deletionist Reading: Notability as Epistemic Quality Filter
 *   domain: digital_commons_governance/knowledge_infrastructure/platform_constitutionalism
 *
 * SUMMARY:
 *   This constraint story models the deletionist reading of Wikipedia's
 *   notability guideline (WP:N) as a coordination Rope: a genuine
 *   collective-action solution to the quality-control problem of an open
 *   encyclopedia. From this reading's structural position, the constraint is
 *   low-extraction, low-suppression coordination that benefits the global
 *   readership by preventing commons degradation. Spam, vanity, and
 *   promotional content are justly excluded — they are not 'victims' but the
 *   very degradation the filter prevents. The constraint requires active
 *   enforcement through the Articles for Deletion (AfD) process, but
 *   enforcement is community-governed and transparent. The claimed type
 *   (rope) and authored metrics are independent facts: the deletionist
 *   reading claims rope; the metrics describe a low-extraction, actively
 *   enforced coordination mechanism. The engine will compute per-seat
 *   classifications from the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deletionist_reading, 0.12).
domain_priors:suppression_score(notability_guidelines__deletionist_reading, 0.18).
domain_priors:theater_ratio(notability_guidelines__deletionist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deletionist_reading, rope).
narrative_ontology:human_readable(notability_guidelines__deletionist_reading, "WP:N Deletionist Reading: Notability as Epistemic Quality Filter").
narrative_ontology:topic_domain(notability_guidelines__deletionist_reading, "digital_commons_governance/knowledge_infrastructure/platform_constitutionalism").

domain_priors:requires_active_enforcement(notability_guidelines__deletionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deletionist_reading, 'f533b8df-6b1a-4afa-9e62-1bc4ef57163c').
narrative_ontology:cs_kernel_codification('f533b8df-6b1a-4afa-9e62-1bc4ef57163c', formalized).
narrative_ontology:cs_authority_grounding('f533b8df-6b1a-4afa-9e62-1bc4ef57163c', practice).
narrative_ontology:cs_interpretation_layer_present('f533b8df-6b1a-4afa-9e62-1bc4ef57163c').
narrative_ontology:cs_reading_relation('f533b8df-6b1a-4afa-9e62-1bc4ef57163c', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f533b8df-6b1a-4afa-9e62-1bc4ef57163c', notability_guidelines__deliberative_reading, influences).
narrative_ontology:cs_axiom('f533b8df-6b1a-4afa-9e62-1bc4ef57163c', foundational, notability_as_quality_threshold).
narrative_ontology:cs_axiom_status(notability_as_quality_threshold, holdable).
narrative_ontology:cs_axiom_grounding('f533b8df-6b1a-4afa-9e62-1bc4ef57163c', notability_as_quality_threshold, conventional).
narrative_ontology:cs_axiom('f533b8df-6b1a-4afa-9e62-1bc4ef57163c', foundational, reliable_sources_as_epistemic_gate).
narrative_ontology:cs_axiom_status(reliable_sources_as_epistemic_gate, holdable).
narrative_ontology:cs_axiom_grounding('f533b8df-6b1a-4afa-9e62-1bc4ef57163c', reliable_sources_as_epistemic_gate, conventional).
narrative_ontology:cs_reference_frame('f533b8df-6b1a-4afa-9e62-1bc4ef57163c', early_wikipedia_quality_crisis).
narrative_ontology:cs_drift_state('f533b8df-6b1a-4afa-9e62-1bc4ef57163c', contemporary_afd_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f533b8df-6b1a-4afa-9e62-1bc4ef57163c', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deletionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, readership).
narrative_ontology:constraint_vindicates(notability_guidelines__deletionist_reading, encyclopedic_quality_requires_verifiability).
narrative_ontology:constraint_vindicates(notability_guidelines__deletionist_reading, reliable_sources_prevent_commons_degradation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% General readers who rely on Wikipedia as a trustworthy reference work. They benefit from the notability filter because it prevents the encyclopedia from being diluted by promotional, fringe, or unverifiable content. Their exit is mobile — they can use other reference works, but Wikipedia's scale and accessibility make it the primary general reference for most.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, readership, beneficiary,
    organized, generational, mobile, global).

% Experienced Wikipedia editors who participate in Articles for Deletion (AfD) discussions and maintain notability standards. They set the agenda by nominating articles for deletion, arguing for inclusion based on sourcing, and shaping consensus. Their exit is constrained — they have invested years in the project and its norms; leaving means abandoning accumulated reputation and community ties.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, core_editors, agenda_setter,
    organized, biographical, constrained, global).

% Wikipedia administrators who close AfD discussions, enforce deletion decisions, and interpret notability policy. They hold institutional power within the platform's governance structure. Their exit is similarly constrained by deep investment in the platform's institutional framework.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, administrators, agenda_setter,
    institutional, biographical, constrained, global).

% Subjects, communities, and knowledge traditions that fail to meet notability thresholds — typically because they lack coverage in conventional reliable sources (mainstream media, academic publishing, established reference works). This includes oral history traditions, marginalized communities' knowledge, emerging fields before they gain institutional recognition, and topics from the Global South underrepresented in Western source ecosystems. They are structurally excluded from the encyclopedia and have no voice in AfD discussions about their own topics.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, excluded_topics, excluded,
    powerless, immediate, trapped, global).

% Academics studying peer production, librarians, educators, and policy analysts who observe Wikipedia's governance from outside. They evaluate whether notability policy achieves its stated coordination function or operates as exclusionary gatekeeping. Their analytical exit means they can shift attention to other platforms or research questions.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, researchers_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the quality-control problem for a globally editable encyclopedia: without a notability threshold, the commons would be overwhelmed by spam, vanity articles, promotional content, and unverifiable claims, destroying reader trust and editorial sustainability. The filter coordinates millions of contributors by establishing a shared, source-based standard for what belongs in the encyclopedia.
% TRANSFER_FUNCTION: Moves editorial attention and article-space from marginal/unverifiable topics to notable/verifiable ones. Transfers the burden of proof to article creators: they must demonstrate notability through independent reliable sources before an article is kept. Transfers deletion labor to the community via AfD process rather than centralized moderation.
% ABSENT_VOICES: Marginalized communities whose knowledge traditions do not generate conventional reliable sources (oral histories, indigenous knowledge, community memory); niche experts in emerging fields before institutional recognition; Global South topics underrepresented in Western source ecosystems; subjects of systemic bias in source availability. These voices would object to exclusion but are structurally absent from AfD discussions and policy formation.
% DISAPPEARANCE_RATIONALE: If the notability filter vanished overnight, Wikipedia would be flooded with promotional content, vanity pages, fringe theories, and unverifiable claims within weeks. Reader trust would collapse; the project's utility as a reference work would degrade; core editors would abandon the project due to unsustainable cleanup burden. The encyclopedia would reorganize into something unrecognizable — likely a spam-filled directory or a heavily moderated walled garden.
% FOUNDING_PROBLEM: Early Wikipedia (2001-2005) had no quality gate: anyone could create articles on anything. This led to rampant vandalism, self-promotion, hoax articles, and unverifiable claims. The notability guideline emerged from community consensus (2006) as a response to the inability to maintain quality at scale without a shared inclusion standard.
% FOUNDING_PROBLEM_CORROBORATION: External corroboration comes from: (1) Academic researchers of peer production (e.g., Forte & Bruckman, Halfaker et al.) documenting that notability policy correlates with sustained editor retention and article quality; (2) Librarians and educators who cite Wikipedia's verifiability/notability standards as why they conditionally trust it as a tertiary source; (3) Comparative analysis of other open wikis without notability policies (e.g., early Wikia/Fandom wikis) showing quality collapse. No corroboration from excluded communities — their absence is the structural gap.
narrative_ontology:disappearance_verdict(notability_guidelines__deletionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deletionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deletionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(notability_guidelines__deletionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deletionist_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__deletionist_reading_tests).
:- end_tests(notability_guidelines__deletionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the constraint does not transfer value from one group to another — it filters content based on verifiability. Suppression is low (0.18) because excluded topics can publish elsewhere (personal websites, specialized wikis, academic journals, social media); the constraint only governs Wikipedia's article space. Theater ratio is low (0.15) because AfD discussions are functional deliberations, not performative rituals — most nominations result in clear keep/delete outcomes based on sourcing. Accessibility collapse is moderate (0.35) because while excluded topics cannot enter Wikipedia, they retain full alternative publication avenues. Resistance is low (0.22) because the policy enjoys broad consensus among active editors and aligns with the project's founding mission.
 *
 * PERSPECTIVAL GAP:
 *   The deletionist seat (agenda_setters + beneficiaries) experiences this as genuine coordination: a shared standard that makes the encyclopedia work. The inclusionist and deliberative readings (sibling constraints) experience the same structure as exclusionary gatekeeping or procedural theater. The engine computes this divergence from the structural data — the authored claim does not adjudicate it. The gap is real: from inside the deletionist frame, notability is a quality threshold; from outside, it can appear as a boundary that reproduces systemic source bias.
 *
 * DIRECTIONALITY LOGIC:
 *   Readership is the structural beneficiary (d ~ 0.1): they gain a trustworthy reference work without bearing costs. Core editors and administrators are agenda_setters with constrained exit (d ~ 0.4-0.5): they invest labor to maintain the filter but also benefit from a sustainable project. Excluded_topics are structurally excluded (not payers) — they bear no extraction because they never had a claim to Wikipedia space; their inability to enter is the filter functioning, not extraction. The directionality derivation from beneficiary declarations + exit options produces low d for beneficiaries, moderate d for agenda_setters, and the excluded seat is not a payer in this reading's frame.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (quality control at scale) remains live — Wikipedia's growth has only increased the need for a shared inclusion standard. The constraint has not atrophied into a piton: theater_ratio remains low, enforcement is functional, and the coordination problem it solves persists. Mandatrophy is resolved in the negative: the mandate has not outlived its function. The slow rise in theater_ratio (0.08→0.15) and suppression_requirement (0.12→0.18) over 18 years bears monitoring — it may indicate creeping proceduralism — but current values remain in rope territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_reading_of_kernel,
    'This constraint is one reading (deletionist_reading) of the contested kernel ''notability_guidelines''. What would the sibling readings (inclusionist_reading, deliberative_reading) change structurally?',
    'Author the sibling constraint stories and compare their beneficiary/victim structures, extractiveness values, and claimed types. The kernel contest is resolved by generating all three readings as separate ε-invariant constraints linked via network.affects_constraints.',
    'If inclusionist_reading authors victims (excluded_topics as payers) and higher extractiveness, the kernel contains a genuine structural contest: the same policy label covers a Rope (this reading) and a Snare/Tangled Rope (inclusionist reading). If deliberative_reading authors high theater_ratio and contested founding_problem_status, the kernel contains a process/outcome split.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_reading_of_kernel, conceptual, 'Committer frame: this story is one reading of a kernel; sibling readings instantiate different constraints.').

omega_variable(
    epistemic_gatekeeping_ambiguity,
    'Does the reliable-sources requirement systematically exclude marginalized knowledge traditions that do not produce conventional reliable sources, or is this a feature-not-bug of epistemic quality filtering?',
    'Empirical audit of AfD outcomes for topics from marginalized communities vs. mainstream topics, controlling for source availability. Compare deletion rates for topics with equivalent source coverage but different community origins.',
    'If systematic exclusion is confirmed, the deletionist reading''s ''no victim set'' claim is structurally false — excluded_topics would be payers, raising extractiveness and shifting claimed_type toward tangled_rope or snare. If exclusion correlates strictly with source availability independent of community origin, the deletionist reading''s frame holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_gatekeeping_ambiguity, empirical, 'Whether the epistemic gate (reliable sources) functions as a neutral quality filter or a structural exclusion mechanism.').

omega_variable(
    notability_threshold_drift,
    'Has the notability threshold drifted upward over time (requiring more sources, higher-profile coverage) such that the constraint''s extractiveness has increased while its claimed coordination function remains constant?',
    'Longitudinal analysis of AfD keep/delete ratios, source-count requirements in deletion discussions, and notability sub-guideline proliferation (WP:NBIO, WP:NORG, etc.) over the interval 2006-2024.',
    'If threshold drift is confirmed, the measurement series understates extraction — base_extractiveness at interval end would be higher than authored. This would support the deliberative_reading''s claim that notability boundaries evolve through practice, and the inclusionist_reading''s claim of creeping gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(notability_threshold_drift, empirical, 'Whether the notability standard has quietly tightened, increasing effective extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deletionist_reading, 2006, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t2006, notability_guidelines__deletionist_reading, theater_ratio, 2006, 0.08).
narrative_ontology:measurement(nota_tr_t2010, notability_guidelines__deletionist_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(nota_tr_t2014, notability_guidelines__deletionist_reading, theater_ratio, 2014, 0.12).
narrative_ontology:measurement(nota_tr_t2018, notability_guidelines__deletionist_reading, theater_ratio, 2018, 0.14).
narrative_ontology:measurement(nota_tr_t2022, notability_guidelines__deletionist_reading, theater_ratio, 2022, 0.15).
narrative_ontology:measurement(nota_tr_t2024, notability_guidelines__deletionist_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(nota_be_t2006, notability_guidelines__deletionist_reading, base_extractiveness, 2006, 0.08).
narrative_ontology:measurement(nota_be_t2010, notability_guidelines__deletionist_reading, base_extractiveness, 2010, 0.09).
narrative_ontology:measurement(nota_be_t2014, notability_guidelines__deletionist_reading, base_extractiveness, 2014, 0.1).
narrative_ontology:measurement(nota_be_t2018, notability_guidelines__deletionist_reading, base_extractiveness, 2018, 0.11).
narrative_ontology:measurement(nota_be_t2022, notability_guidelines__deletionist_reading, base_extractiveness, 2022, 0.12).
narrative_ontology:measurement(nota_be_t2024, notability_guidelines__deletionist_reading, base_extractiveness, 2024, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t2006, notability_guidelines__deletionist_reading, suppression_requirement, 2006, 0.12).
narrative_ontology:measurement(nota_su_t2010, notability_guidelines__deletionist_reading, suppression_requirement, 2010, 0.15).
narrative_ontology:measurement(nota_su_t2014, notability_guidelines__deletionist_reading, suppression_requirement, 2014, 0.16).
narrative_ontology:measurement(nota_su_t2018, notability_guidelines__deletionist_reading, suppression_requirement, 2018, 0.17).
narrative_ontology:measurement(nota_su_t2022, notability_guidelines__deletionist_reading, suppression_requirement, 2022, 0.18).
narrative_ontology:measurement(nota_su_t2024, notability_guidelines__deletionist_reading, suppression_requirement, 2024, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deletionist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(notability_guidelines__deletionist_reading, 0.02).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__inclusionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__deliberative_reading).

% DUAL FORMULATION NOTE:
% This constraint (deletionist_reading) and its siblings (inclusionist_reading, deliberative_reading) form the notability_guidelines constraint family. They share the kernel 'notability_guidelines' but instantiate different constraints with different ε values, beneficiary/victim structures, and claimed types. The deletionist reading claims Rope (low ε, readership beneficiary, no victims). The inclusionist reading likely claims Tangled Rope or Snare (higher ε, excluded_topics as victims). The deliberative reading likely claims Scaffold or Tangled Rope (process-oriented, higher theater_ratio). All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
