% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__temporal_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__temporal_accommodation_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__temporal_accommodation_reading
 *   human_readable: Temporal Accommodation Reading: Suspended Polygamy Doctrine Under Federal Pressure
 *   domain: religious_law/political_theology
 *
 * SUMMARY:
 *   The 1890 Manifesto (Official Declaration 1) suspended the practice of
 *   polygamy while preserving the doctrine that it is an eternal principle.
 *   This reading frames the suspension as a temporary political
 *   accommodation: the eternal covenant remains valid; obedience to federal
 *   law takes precedence in practice; the doctrine awaits restoration when
 *   political constraints lift. The reading is contestable: it could be
 *   interpreted as a doctrinal equivocation that preserves male hierarchical
 *   authority while appearing to concede to federal pressure, or as a genuine
 *   theological commitment to two competing authorities (eternal law and
 *   temporal jurisdiction). The constraint story models this reading's
 *   structure, not its truth-value.
 *
 * KEY AGENTS:
 *   - Church institutional authority: sets the suspension policy, maintains the doctrine, administers which revelations are binding
 *   - Polygamous practitioners: benefit from doctrinal vindication while suspending practice to comply with law
 *   - Women in polygamous marriages: bear the cost of legal invisibility and structural subordination without recourse
 *   - Federal government: excluded from doctrinal adjudication, can only enforce external practice prohibition
 *   - Theological dissenters (fundamentalists): reject the suspension as betrayal, maintain the practice clandestinely
 *   - Non-polygamous members: benefit from institutional legitimacy secured by Manifesto acceptance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, 0.62).
domain_priors:suppression_score(eternal_marriage_covenant__temporal_accommodation_reading, 0.71).
domain_priors:theater_ratio(eternal_marriage_covenant__temporal_accommodation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__temporal_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__temporal_accommodation_reading, "Temporal Accommodation Reading: Suspended Polygamy Doctrine Under Federal Pressure").
narrative_ontology:topic_domain(eternal_marriage_covenant__temporal_accommodation_reading, "religious_law/political_theology").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__temporal_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__temporal_accommodation_reading, '68627cf9-6e02-41a1-81fb-d314ab81569d').
narrative_ontology:cs_kernel_codification('68627cf9-6e02-41a1-81fb-d314ab81569d', fixed_text).
narrative_ontology:cs_authority_grounding('68627cf9-6e02-41a1-81fb-d314ab81569d', extraction).
narrative_ontology:cs_interpretation_layer_present('68627cf9-6e02-41a1-81fb-d314ab81569d').
narrative_ontology:cs_reading_relation('68627cf9-6e02-41a1-81fb-d314ab81569d', eternal_marriage_covenant__immutable_commandment_reading, coexists_with).
narrative_ontology:cs_reading_relation('68627cf9-6e02-41a1-81fb-d314ab81569d', eternal_marriage_covenant__prophetic_override_reading, influences).
narrative_ontology:cs_axiom('68627cf9-6e02-41a1-81fb-d314ab81569d', foundational, polygamy_eternally_binding_doctrine).
narrative_ontology:cs_axiom_status(polygamy_eternally_binding_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('68627cf9-6e02-41a1-81fb-d314ab81569d', polygamy_eternally_binding_doctrine, deontological).
narrative_ontology:cs_axiom('68627cf9-6e02-41a1-81fb-d314ab81569d', foundational, temporal_law_overrides_practice_not_doctrine).
narrative_ontology:cs_axiom_status(temporal_law_overrides_practice_not_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('68627cf9-6e02-41a1-81fb-d314ab81569d', temporal_law_overrides_practice_not_doctrine, conventional).
narrative_ontology:cs_reference_frame('68627cf9-6e02-41a1-81fb-d314ab81569d', eternal_polygamous_covenant_valid).
narrative_ontology:cs_drift_state('68627cf9-6e02-41a1-81fb-d314ab81569d', post_manifesto_federal_settlement, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('68627cf9-6e02-41a1-81fb-d314ab81569d', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, church_institutional_authority).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, polygamous_practitioners_under_covenant).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, female_members_in_polygamous_marriages).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, federal_territory_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, non_polygamous_church_members).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, polygamous_practitioners_under_covenant).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, theological_dissenters_fundamentalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and enforces the Manifesto (1890) suspending polygamous practice while preserving the doctrine that polygamy is an eternal principle. Administers the church's doctrinal apparatus and decides which revelations are binding at any given time. Claims obedience to federal law as the governing constraint while maintaining that the underlying covenant remains valid and will be reinstated when political conditions permit. Controls the narrative framing of suspension as temporary accommodation rather than doctrinal change.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, church_institutional_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Have entered into polygamous marriage covenants framed as eternal and necessary for exaltation. The reading permits them to continue honoring those covenants spiritually while suspending outward practice to comply with federal law. They gain doctrinal vindication (the covenant is still eternal) and the church's institutional support for their spiritual status, while bearing the cost of legal invisibility and social concealment of their family arrangements. Exit would mean renouncing the covenant and accepting damnation within the faith's cosmology.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, polygamous_practitioners_under_covenant, beneficiary,
    powerful, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__temporal_accommodation_reading, polygamous_practitioners_under_covenant, payer).

% Are bound into polygamous arrangements under the suspended-but-eternal doctrine. They are subordinated under a hierarchy framed as divinely ordained and eternally binding. The suspension allows them legal recourse through federal courts if they seek dissolution, but doing so means expulsion from the faith community and loss of their social/spiritual/familial identity. Their situation is one of structural constraint: formally legal options exist but carry total social cost.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, female_members_in_polygamous_marriages, payer,
    moderate, generational, constrained, regional).

% Applied territorial law prohibiting polygamy as a condition of statehood and settlement. The Manifesto's declaration of compliance is accepted as settlement of the federal-church conflict, though federal authorities remain structurally excluded from the church's internal doctrinal adjudication. They cannot prevent the doctrine from being preserved as eternally valid, only the overt practice. Their enforcement power is limited to external behavior.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, federal_government, excluded,
    institutional, generational, trapped, national).

% Benefit from the church's institutional legitimacy and legal standing secured by the Manifesto's acceptance of federal law. They also benefit from the suppression of an arrangement that competed with monogamous marriage for prestige and resources within the faith. They carry no direct cost from the doctrine's eternal validity because it is suspended.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, non_polygamous_church_members, beneficiary,
    organized, generational, mobile, global).

% Reject the Manifesto's suspension as a betrayal of eternal doctrine. They argue that obedience to the eternal covenant supersedes obedience to federal law, creating schism. They bear the cost of expulsion and marginalization while maintaining that the suspended practice is the true path to exaltation. The reading's framework (eternal doctrine + temporal suspension) denies them standing within the institutional church.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, theological_dissenters_fundamentalists, payer,
    powerless, civilizational, identity_locked, regional).

% From outside the faith, they observe that the Manifesto permits the doctrine to persist while suspending practice, leaving women in existing polygamous marriages without institutional reform. The reading is analyzed as an accommodation that preserves male authority while appearing to concede to federal pressure. Their voice is excluded from the church's internal decision-making apparatus.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, women_rights_advocates_external, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__temporal_accommodation_reading, church_institutional_authority).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__temporal_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reading coordinates obedience to two competing authorities — federal law and eternal divine law — by distinguishing temporal practice (suspended, subject to federal jurisdiction) from eternal doctrine (dormant but valid, pending restoration). This coordination permits the institutional church to claim loyalty to both without doctrinal revision.
% TRANSFER_FUNCTION: Moves authority over the practice of polygamy from the church to federal government (practice suspended by federal law), while moves authority over the doctrine's validity back to the church (doctrine remains eternally binding in spiritual cosmology). Moves the cost of suspension onto women in existing polygamous arrangements and onto dissenters who maintain the practice.
% ABSENT_VOICES: Women in existing polygamous marriages at the time of the Manifesto (1890) were not consulted; their voices are structurally excluded from the decision. Fundamentalist dissenters who reject the suspension are also excluded from the institutional authority structure that frames it as legitimate. Federal territorial subjects who object to the persistence of the doctrine itself have no standing in church deliberation.
% DISAPPEARANCE_RATIONALE: The reading's institutional beneficiaries claim that the doctrine's eternal validity would restore the practice if federal pressure lifted — the arrangement is framed as temporarily suspended, not abolished. Dissenters argue the practice is already dead and the doctrine is theological theater. Federal observers note the reading permits evasion: the doctrine is preserved for future restoration. The verdict depends on whether the eternal-but-suspended frame is read as commitment to restoration or as protective fiction.
% FOUNDING_PROBLEM: Federal territory law prohibited polygamy as a condition of admission to the Union and ecclesiastical legitimacy. The church faced state pressure that threatened institutional survival and statehood. The reading solves the federal problem (apparent compliance with law) while attempting to preserve the doctrinal claim (eternal validity).
% FOUNDING_PROBLEM_CORROBORATION: Federal officials accepted the Manifesto as settling the polygamy question for purposes of statehood (1896). Church historians and doctrine interpreters attest the founding problem—federal territory law—is resolved by the suspension. However, contemporary women's rights observers and theological dissenters attest that the founding problem (whether eternal polygamy is compatible with federal law) is NOT resolved, only suspended; it remains live as long as the doctrine is valid. The corroboration is mixed and seat-dependent: beneficiaries attest the problem is solved; victims and dissenters attest it is not.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__temporal_accommodation_reading, contested).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__temporal_accommodation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eternal_marriage_covenant__temporal_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The reading is classified as Tangled Rope because it combines a genuine coordination function (negotiating obedience to two competing authorities — eternal law and federal law) with asymmetric extraction: the church authority gains legitimacy and institutional survival; polygamous practitioners retain doctrinal status and spiritual identity; women in polygamous marriages are locked into subordination without institutional remedy. Extraction is moderate-to-high (0.62 at interval end) because the reading preserves the doctrine indefinitely while suspending practice, leaving its asymmetry intact. Suppression is high (0.71) because the arrangement requires active enforcement — maintaining secrecy around continued polygamous covenanting, expelling dissenters, limiting women's exit options. Theater ratio rises from 0.52 to 0.60 then stabilizes at 0.58 because the Manifesto itself is performative (a declaration of compliance accepted by federal authorities) but the underlying structure (eternal doctrine + temporal suspension) requires continuous doctrinal management rather than genuine functional change. The measurement series shows theater rising into the 1920s-1930s (peak management effort) then stabilizing as the arrangement becomes institutionalized and routine.
 *
 * PERSPECTIVAL GAP:
 *   The institutional authority sees the reading as a genuine coordination accomplishment: obedience to both eternal law (preserved in doctrine) and temporal law (practiced suspension) without doctrinal revision. The polygamous practitioners see it as a protective framework: their covenants remain eternally valid, their practice suspended by external force, their exit foreclosed by the doctrine itself. Women in polygamous marriages see it as entrapment: the doctrine remains eternally binding, the practice is suspended (not forbidden), their exit requires renouncing eternal status. Dissenters see it as institutional betrayal: the suspension contradicts the eternal claim. Federal observers see it as evasion: the doctrine is preserved for future restoration. The engine's per-seat computation should show these divergent readings directly from the structural data (beneficiary/victim + exit options + power atoms).
 *
 * DIRECTIONALITY LOGIC:
 *   The church institutional authority sits at the beneficiary end (d ≈ 0.1) — it gains institutional legitimacy, statehood, and legal standing without renouncing its doctrinal claims. Polygamous practitioners sit near symmetric (d ≈ 0.5) — they benefit from doctrinal vindication but pay the cost of legal invisibility and social concealment. Women in polygamous marriages sit at the target end (d ≈ 0.85) — they bear the extraction's full weight: subordination under an eternally valid doctrine, constrained exit (leaving means community expulsion and spiritual damnation), no voice in the decision that suspended the practice without reforming the doctrine. Federal government is excluded rather than coordinated. Theological dissenters are targets (d ≈ 0.9) — they are expunged from the community for rejecting the reading's frame.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading does not exhibit mandatrophy (the founding problem is not dead). The founding problem — federal territorial law prohibiting polygamy — is genuinely solved by the Manifesto's suspension for purposes of federal compliance and statehood. However, the reading leaves the underlying structural problem unresolved: whether eternal polygamous doctrine is compatible with monogamous federal law. The suspension is framed as temporary, which preserves the tension rather than resolving it. Mandatrophy would arise only if the reading claimed the founding problem was permanently solved while the doctrine remained eternally valid — a logical contradiction that the reading avoids by framing suspension as temporary. The reading is sustainable as long as the restoration condition (political constraints lifting) remains plausible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_doctrinal_change,
    'Is the 1890 Manifesto a genuine suspension of practice (doctrine preserved, practice temporarily suspended) or a doctrinal revision disguised as suspension?',
    'Examine church leadership''s statements 1890-1950: do they repeatedly affirm the doctrine as eternally valid and suspension as temporary, or do their framing and emphasis shift toward acceptance of the doctrine''s invalidity? Look for changes in how the doctrine is taught and whether restoration language persists or fades.',
    'If genuine suspension (as the reading claims), the constraint is tangled_rope (coordination + extraction). If doctrinal revision (contra the reading''s claim), the constraint would be snare (extraction under the cover of coordination). The classification hinges on this factual and interpretive question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_vs_doctrinal_change, conceptual, 'Whether the doctrine is suspended (temporally contingent) or revised (permanently changed).').

omega_variable(
    restoration_conditionality,
    'Under what conditions, if any, does the eternal doctrine restore to practice? Is the condition tied to political change (external contingency), prophetic decision (institutional discretion), or is the doctrine functionally dormant indefinitely?',
    'Examine official church statements on the doctrine''s restoration timeline and conditions. Compare the 1890 Manifesto''s language (restoration when federal pressure lifts) with subsequent official guidance (1950s onward). Track whether the doctrine is taught as actively awaiting restoration or as superseded.',
    'If the restoration condition is contingent on external politics (federal pressure lifting), the reading frames suspension as temporary and the doctrine as live. If the condition is vague or never articulated, the reading collapses into permanent suppression disguised as suspension — which would shift the constraint toward snare. If the condition is formally renounced (as in the 1978 Official Declaration 2 regarding race), the doctrine dies and the constraint becomes historical rather than persistent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_conditionality, empirical, 'Whether the suspended doctrine has a live restoration condition or is functionally permanent.').

omega_variable(
    women_agency_and_coercion,
    'Are women in polygamous marriages under this reading bound by the eternal doctrine (making exit impossible without spiritual damnation) or is exit a real option made costly by social pressure?',
    'Examine historical records of women''s responses: do they describe themselves as bound by eternal doctrine or as constrained by social/economic factors? Compare exit rates before and after the Manifesto''s suspension to assess whether the suspension changed women''s structural exit options.',
    'If exit is structurally impossible (doctrine-enforced), the suppression metric should be higher and resistance lower than authored; the constraint is more snare-like. If exit is costly but possible (socially enforced), the authored suppression and resistance metrics stand. The classification is sensitive to this distinction because identity_locked exit suggests the doctrine itself is the binding constraint, not just social pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(women_agency_and_coercion, empirical, 'The degree to which women''s subordination in polygamy is doctrine-enforced or socially-enforced under this reading.').

omega_variable(
    sibling_reading_empirical_divergence,
    'The immutable_commandment_reading and prophetic_override_reading claim different things about the doctrine''s binding force. Which reading''s empirical claims about women''s agency, doctrinal enforcement, and institutional persistence are supported by historical evidence?',
    'Corpus-level comparison: generate the immutable_commandment_reading and prophetic_override_reading as separate constraint stories. Compare their claimed types and metrics to this reading. Examine historical sources to see which reading''s description of the doctrine''s bindingness matches women''s lived experience in polygamous marriages.',
    'Sibling readings are not identical constraints viewed from different angles — they are different constraints with different ε values and different beneficiary structures. If the immutable_commandment_reading (doctrine eternally binding) produces higher extraction and lower accessibility_collapse than this accommodation reading, the difference is not perspectival — it reflects different structural claims. The family of three readings enables the corpus to detect which reading''s structural claim is empirically true.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_empirical_divergence, conceptual, 'The kernel contest: which reading''s structural claims are empirically supported?').

omega_variable(
    federal_pressure_vs_internal_choice,
    'Did the Manifesto emerge from federal pressure (the reading''s frame) or from internal church evolution toward monogamy? Is the suspension genuinely contingent on federal law, or would the church have voluntarily renounced polygamy regardless?',
    'Historical counterfactual: examine church leadership''s pre-Manifesto statements about polygamy''s necessity. Compare the Manifesto''s language (obedience to law of land takes precedence) with statements in private correspondence. Look for evidence of ideological commitment to monogamy before federal pressure intensified (1880s-1890s).',
    'If suspension is genuinely contingent on federal pressure (the reading''s claim), the restoration condition is live and the reading''s framing (temporary accommodation) is accurate. If the church had begun shifting away from polygamy regardless, the Manifesto is post-hoc justification and the reading is theater-heavy. The distinction affects the claim/metric divergence: a reading that frames the constraint as genuinely temporary (contingent on external pressure) but authors it as stable (theater_ratio high) has misaligned claim and metrics in a specific way.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_pressure_vs_internal_choice, empirical, 'Whether federal pressure is the genuine cause of suspension or post-hoc justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__temporal_accommodation_reading, 1890, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1890, 0.52).
narrative_ontology:measurement_basis(eter_tr_t1890, observed).
narrative_ontology:measurement(eter_tr_t1905, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1905, 0.56).
narrative_ontology:measurement_basis(eter_tr_t1905, observed).
narrative_ontology:measurement(eter_tr_t1920, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1920, 0.6).
narrative_ontology:measurement_basis(eter_tr_t1920, observed).
narrative_ontology:measurement(eter_tr_t1935, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1935, 0.59).
narrative_ontology:measurement_basis(eter_tr_t1935, observed).
narrative_ontology:measurement(eter_tr_t1950, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1950, 0.58).
narrative_ontology:measurement_basis(eter_tr_t1950, observed).

% Extraction over time
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1890, 0.58).
narrative_ontology:measurement_basis(eter_be_t1890, observed).
narrative_ontology:measurement(eter_be_t1905, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1905, 0.61).
narrative_ontology:measurement_basis(eter_be_t1905, observed).
narrative_ontology:measurement(eter_be_t1920, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1920, 0.64).
narrative_ontology:measurement_basis(eter_be_t1920, observed).
narrative_ontology:measurement(eter_be_t1935, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1935, 0.62).
narrative_ontology:measurement_basis(eter_be_t1935, observed).
narrative_ontology:measurement(eter_be_t1950, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1950, 0.62).
narrative_ontology:measurement_basis(eter_be_t1950, observed).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1890, 0.68).
narrative_ontology:measurement_basis(eter_su_t1890, observed).
narrative_ontology:measurement(eter_su_t1905, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1905, 0.7).
narrative_ontology:measurement_basis(eter_su_t1905, observed).
narrative_ontology:measurement(eter_su_t1920, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1920, 0.73).
narrative_ontology:measurement_basis(eter_su_t1920, observed).
narrative_ontology:measurement(eter_su_t1935, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1935, 0.71).
narrative_ontology:measurement_basis(eter_su_t1935, observed).
narrative_ontology:measurement(eter_su_t1950, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1950, 0.71).
narrative_ontology:measurement_basis(eter_su_t1950, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__temporal_accommodation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__temporal_accommodation_reading, 0.14).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant__prophetic_override_reading).

% DUAL FORMULATION NOTE:
% The eternal_marriage_covenant kernel generates three distinct constraints, each instantiating a different reading of the same doctrinally codified claim. The temporal_accommodation_reading (this story) frames the 1890 Manifesto as suspension pending restoration; the immutable_commandment_reading frames the doctrine as eternally binding regardless of suspension; the prophetic_override_reading frames continuing revelation as the mechanism of authority. All three read the same kernel (D&C 132) and the same historical event (the Manifesto), but structure its significance and binding force differently. The readings generate different beneficiary/victim sets and different ε values — they are not perspectives on a single constraint, but separate constraints with incompatible structural claims. The family link enables the corpus to model the kernel contest and detect which reading's structural claim is empirically sustained.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eternal_marriage_covenant__temporal_accommodation_reading, powerful, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
