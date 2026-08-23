% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__durable_separation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__durable_separation_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: herem_command_dt7__durable_separation_reading
 *   human_readable: Herem as Timeless Divine Mandate for Identity Preservation through Categorical Separation
 *   domain: biblical_hermeneutics/religious_ethics/commitment_system
 *
 * SUMMARY:
 *   The durable_separation_reading of herem_command_dt7 interprets the
 *   biblical ḥerem (devotion-to-destruction/ban) texts as encoding a timeless
 *   divine mandate: covenant identity requires bounded membership maintained
 *   through categorical separation from designated outsider populations. This
 *   reading claims the coordination function of identity preservation across
 *   generations, but its operation extracts heavily on intermarriage autonomy
 *   (covenant members cannot freely choose partners across the boundary),
 *   constructs an expansive victim set (all non-covenant groups are potential
 *   contamination threats subject to violence or exclusion), and legitimates
 *   violence through divine command obedience. The reading is advanced by
 *   interpretive authorities within rabbinic, patristic, and confessional
 *   traditions who administer the boundary definitions. Sibling readings
 *   (contextual supersession, allegorical displacement) contest the
 *   timelessness, the ethnic referent, and the violence legitimation
 *   respectively.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, 0.78).
domain_priors:suppression_score(herem_command_dt7__durable_separation_reading, 0.85).
domain_priors:theater_ratio(herem_command_dt7__durable_separation_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__durable_separation_reading, rope).
narrative_ontology:human_readable(herem_command_dt7__durable_separation_reading, "Herem as Timeless Divine Mandate for Identity Preservation through Categorical Separation").
narrative_ontology:topic_domain(herem_command_dt7__durable_separation_reading, "biblical_hermeneutics/religious_ethics/commitment_system").

domain_priors:requires_active_enforcement(herem_command_dt7__durable_separation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__durable_separation_reading, 'bbddb011-ed58-41db-8a99-4f4e7e1b98b8').
narrative_ontology:cs_kernel_codification('bbddb011-ed58-41db-8a99-4f4e7e1b98b8', formalized).
narrative_ontology:cs_authority_grounding('bbddb011-ed58-41db-8a99-4f4e7e1b98b8', lineage).
narrative_ontology:cs_interpretation_layer_present('bbddb011-ed58-41db-8a99-4f4e7e1b98b8').
narrative_ontology:cs_reading_relation('bbddb011-ed58-41db-8a99-4f4e7e1b98b8', herem_command_dt7__contextual_supersession_reading, forecloses).
narrative_ontology:cs_reading_relation('bbddb011-ed58-41db-8a99-4f4e7e1b98b8', herem_command_dt7__allegorical_displacement_reading, forecloses).
narrative_ontology:cs_axiom('bbddb011-ed58-41db-8a99-4f4e7e1b98b8', foundational, herem_perpetual_divine_mandate).
narrative_ontology:cs_axiom_status(herem_perpetual_divine_mandate, holdable).
narrative_ontology:cs_axiom_grounding('bbddb011-ed58-41db-8a99-4f4e7e1b98b8', herem_perpetual_divine_mandate, deontological).
narrative_ontology:cs_axiom('bbddb011-ed58-41db-8a99-4f4e7e1b98b8', foundational, covenant_identity_requires_categorical_separation).
narrative_ontology:cs_axiom_status(covenant_identity_requires_categorical_separation, holdable).
narrative_ontology:cs_axiom_grounding('bbddb011-ed58-41db-8a99-4f4e7e1b98b8', covenant_identity_requires_categorical_separation, deontological).
narrative_ontology:cs_axiom('bbddb011-ed58-41db-8a99-4f4e7e1b98b8', secondary, divine_command_obedience_legitimates_violence_against_designated_outsiders).
narrative_ontology:cs_axiom_status(divine_command_obedience_legitimates_violence_against_designated_outsiders, holdable).
narrative_ontology:cs_axiom_grounding('bbddb011-ed58-41db-8a99-4f4e7e1b98b8', divine_command_obedience_legitimates_violence_against_designated_outsiders, deontological).
narrative_ontology:cs_reference_frame('bbddb011-ed58-41db-8a99-4f4e7e1b98b8', sinai_covenant_formation).
narrative_ontology:cs_drift_state('bbddb011-ed58-41db-8a99-4f4e7e1b98b8', contemporary_universalist_challenge, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('bbddb011-ed58-41db-8a99-4f4e7e1b98b8', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__durable_separation_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, covenant_community_as_corporate_identity).
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, religious_authority_structure).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, designated_outsider_populations).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, covenant_members_seeking_exogamous_marriage).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, dissenting_voices_within_covenant).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, divine_election_requires_bounded_membership).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, categorical_separation_preserves_holiness).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, obedience_to_herem_is_covenant_fidelity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The corporate covenant community receives preserved distinct identity and theological coherence through the herem mandate. Its members experience this as gift and calling. Exit from the identity frame is experienced as apostasy or existential dissolution, not merely institutional departure.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, covenant_community_as_corporate_identity, beneficiary,
    organized, generational, identity_locked, universal).

% Interpretive authorities (rabbinic, patristic, confessional) administer the herem mandate by defining who counts as designated outsider, what constitutes contamination, and how separation is maintained. They derive institutional legitimacy and interpretive control from this administration. Their exit is constrained by vocational identity and institutional position.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, religious_authority_structure, agenda_setter,
    institutional, generational, constrained, universal).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__durable_separation_reading, religious_authority_structure, beneficiary).

% Individual covenant members who form attachment bonds across the herem boundary bear the extraction directly: forbidden marriage, communal discipline, potential exclusion. Their autonomy over intimate association is the primary extraction site. Exit options are constrained — leaving the community severs kinship and identity; staying requires surrender of the relationship.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, covenant_members_seeking_exogamous_marriage, payer,
    moderate, biographical, constrained, universal).

% Populations categorized as 'nations' subject to herem (historically: Canaanites, Amalekites; in durable separation reading: any non-covenant group designated as contamination threat) bear the most extreme extraction: violence, dispossession, or permanent exclusion legitimated as divine obedience. They have no voice in the categorization and no exit from the threat — the constraint defines them as its object.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, designated_outsider_populations, payer,
    powerless, generational, trapped, universal).

% Prophetic, ethical, or hermeneutical voices within the covenant tradition that challenge the durable separation reading (e.g., Jonah, Ruth, prophetic universalism, Paul's 'neither Jew nor Greek') are structurally excluded from the reading's authoritative conversation. They would object to the expansive victim set and violence legitimation but are ruled out of bounds by the reading's own interpretive rules.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, dissenting_voices_within_covenant, excluded,
    moderate, biographical, constrained, universal).

% Sees the full structure: a commitment system that claims timeless divine mandate to coordinate identity through categorical separation, extracting autonomy from insiders and legitimacy/life from outsiders, enforced through interpretive authority that treats the mandate as non-negotiable.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates corporate covenant identity across generations by maintaining a bounded membership perimeter: who is inside the covenant people and who is outside is settled by the herem mandate, preventing assimilation and identity dissolution.
% TRANSFER_FUNCTION: Moves autonomy over marriage, association, and territorial coexistence from designated outsider populations and boundary-crossing covenant members to the covenant community as a corporate entity and its interpretive authorities. Also transfers the legitimation burden for violence from human decision to divine command.
% ABSENT_VOICES: The designated outsider populations themselves — Canaanites, Amalekites, and in the reading's expansive logic any group categorized as contamination threat — are structurally silenced; they cannot contest their designation. Also absent: covenant members who experience the mandate as destructive rather than preservative, and prophetic/universalist traditions within the same textual corpus that are ruled hermeneutically out of bounds.
% DISAPPEARANCE_RATIONALE: If the durable separation reading vanished overnight, the covenant community would lose its primary textual warrant for categorical exclusion and violence against outsiders; marriage and association across former boundaries would become permissible; interpretive authorities would lose a core legitimating mandate; the identity-perimeter maintenance apparatus would require new justification or collapse.
% FOUNDING_PROBLEM: How does a covenant community preserve its distinct identity and theological coherence across generations when surrounded by competing cults, cultures, and political powers that threaten assimilation?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the biblical narrative itself (Deuteronomy 7, Joshua 6-11) and by the interpretive tradition's own self-understanding. However, the claim that this problem requires the *specific* solution of herem-as-timeless-mandate is contested by sibling readings (contextual supersession, allegorical displacement) and by internal prophetic witnesses (Jonah, Isaiah 56, Ruth) that the durable separation reading excludes from its authoritative frame. No corroboration exists outside the benefiting interpretive tradition.
narrative_ontology:disappearance_verdict(herem_command_dt7__durable_separation_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__durable_separation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__durable_separation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(herem_command_dt7__durable_separation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__durable_separation_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__durable_separation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(herem_command_dt7__durable_separation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint transfers autonomy over intimate association and territorial coexistence from designated outsiders and boundary-crossing insiders to the corporate covenant identity and its interpretive authorities. Suppression is higher (0.85) because the constraint's persistence depends on actively excluding rival hermeneutics (prophetic universalism, allegorical readings) and on enforcing the boundary against both outsider presence and insider dissent. Theater ratio is moderate (0.25): the identity-preservation coordination function is real and valued by beneficiaries, but a growing share of enforcement activity defends the violence legitimation and expansive victim set rather than the core identity boundary. Accessibility collapse (0.72) is high because the reading's interpretive rules render alternative framings (contextual, allegorical) unintelligible from inside the frame. Resistance (0.68) is substantial from excluded voices and historical communities that maintained identity without herem violence.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seat (religious authority structure), the constraint is genuine coordination (rope) preserving covenant identity against dissolution. From the payer seats (boundary-crossing insiders, designated outsiders), the same structure operates as enforced extraction with violence legitimation (snare/tangled_rope). The engine computes this divergence from the structural data; the claimed_type 'rope' reflects the reading's self-understanding while the metrics describe its operation.
 *
 * DIRECTIONALITY LOGIC:
 *   The covenant community as corporate identity and the religious authority structure are structural beneficiaries (d near 0.0-0.2): they receive preserved coherence and interpretive control. Covenant members seeking exogamous marriage are payers with constrained exit (d ~0.7): they bear the autonomy extraction directly but remain identity-locked to the community. Designated outsider populations are payers with trapped exit (d ~1.0): they bear violence/exclusion with no voice in the categorization. Dissenting voices are excluded (analytical d but structurally silenced). The analytical observer sees the full extraction gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (identity preservation against assimilation) is contested as still live. The reading claims the problem persists and requires the herem solution; sibling readings and internal prophetic witnesses attest the problem is either solved by other means (contextual supersession) or misidentified (allegorical displacement). The mandate shows signs of mandatrophy: the original conquest-period directive has been extended into a timeless mandate that now serves primarily to legitimate the interpretive authority's boundary control and violence authorization, while the coordination function (identity preservation) is demonstrably achievable by less extractive means (diaspora communities, prophetic universalism). The theater ratio rise over the interval tracks this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_kernel_reading,
    'This constraint is one reading (durable_separation_reading) of the contested kernel herem_command_dt7. What structural elements would change under the sibling readings (contextual_supersession_reading, allegorical_displacement_reading)?',
    'Comparative constraint story generation for each sibling reading; structural delta analysis across the kernel family.',
    'If sibling readings produce substantially different ε, victim sets, or coordination functions, the kernel is structurally polyvalent — the ''herem command'' is not a single constraint but a family. This reading''s high extraction on intermarriage autonomy and expansive victim set would be reading-specific, not kernel-intrinsic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel_reading, conceptual, 'Commitment-system kernel polyvalence: whether herem_command_dt7 instantiates one constraint or a constraint family.').

omega_variable(
    coordination_vs_extraction_boundary,
    'Is the identity-preservation coordination function genuine (the community would dissolve without categorical separation) or is it a cover story for extraction (violence legitimation, autonomy capture) that could be achieved by less extractive means?',
    'Historical analysis of covenant communities that maintained identity without herem-style violence (e.g., diaspora Judaism, early Christian communities); comparative study of boundary-maintenance mechanisms across religious traditions.',
    'If identity preservation is achievable without the expansive victim set and violence legitimation, the coordination story is contingently true but the extraction is structurally unnecessary — pushing classification toward snare. If no alternative exists, tangled_rope is structurally honest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether the coordination function requires the specific extractive mechanisms this reading authorizes.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of dissenting voices and boundary-crossers primarily structural (enforcement by authority) or internalized (covenant members self-police because the identity frame makes alternatives unthinkable)?',
    'Post-exit trajectory study: do former covenant members who reject the durable separation reading continue to experience suppression internally, or does it dissolve with the identity frame?',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint travels with the agent. This affects χ computation for identity_locked exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in identity-locked covenant membership.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__durable_separation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(herem_durable_sep_tr_t0, herem_command_dt7__durable_separation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(herem_durable_sep_tr_t25, herem_command_dt7__durable_separation_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement(herem_durable_sep_tr_t50, herem_command_dt7__durable_separation_reading, theater_ratio, 50, 0.21).
narrative_ontology:measurement(herem_durable_sep_tr_t75, herem_command_dt7__durable_separation_reading, theater_ratio, 75, 0.23).
narrative_ontology:measurement(herem_durable_sep_tr_t100, herem_command_dt7__durable_separation_reading, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(herem_durable_sep_be_t0, herem_command_dt7__durable_separation_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(herem_durable_sep_be_t25, herem_command_dt7__durable_separation_reading, base_extractiveness, 25, 0.72).
narrative_ontology:measurement(herem_durable_sep_be_t50, herem_command_dt7__durable_separation_reading, base_extractiveness, 50, 0.75).
narrative_ontology:measurement(herem_durable_sep_be_t75, herem_command_dt7__durable_separation_reading, base_extractiveness, 75, 0.77).
narrative_ontology:measurement(herem_durable_sep_be_t100, herem_command_dt7__durable_separation_reading, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(herem_durable_sep_su_t0, herem_command_dt7__durable_separation_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(herem_durable_sep_su_t25, herem_command_dt7__durable_separation_reading, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(herem_durable_sep_su_t50, herem_command_dt7__durable_separation_reading, suppression_requirement, 50, 0.81).
narrative_ontology:measurement(herem_durable_sep_su_t75, herem_command_dt7__durable_separation_reading, suppression_requirement, 75, 0.83).
narrative_ontology:measurement(herem_durable_sep_su_t100, herem_command_dt7__durable_separation_reading, suppression_requirement, 100, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__durable_separation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(herem_command_dt7__durable_separation_reading, 0.08).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, herem_command_dt7__contextual_supersession_reading).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, herem_command_dt7__allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% Kernel herem_command_dt7 decomposes into three constraint stories: this durable_separation_reading (high extraction, expansive victim set, violence legitimation), contextual_supersession_reading (low extraction, historically bounded, no perennial victim set), and allegorical_displacement_reading (negligible extraction, spiritualized referent, no ethnic victim set). The ε values differ by a wide margin (0.78 vs ~0.15 vs ~0.02). They share the kernel text but instantiate different constraints with different stakeholder structures. This reading is upstream in the contamination network: its authority claim is cited to resist the sibling readings' revisions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(herem_command_dt7__durable_separation_reading, organized, 0.15).
constraint_indexing:directionality_override(herem_command_dt7__durable_separation_reading, institutional, 0.2).
constraint_indexing:directionality_override(herem_command_dt7__durable_separation_reading, moderate, 0.68).
constraint_indexing:directionality_override(herem_command_dt7__durable_separation_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
