% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__continuationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__continuationist_reading, []).

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
 *   constraint_id: divine_marriage_command__continuationist_reading
 *   human_readable: Divine Marriage Command (Continuationist Reading: Polygamy Theologically Valid, Manifesto as Prudential Suspension)
 *   domain: religious/political theology
 *
 * SUMMARY:
 *   The 1890 Manifesto suspended polygamy, officially to comply with federal
 *   law. The continuationist reading holds that this suspension is prudential
 *   accommodation to external duress, not doctrinal rescission — the original
 *   divine command remains in effect. Fundamentalist splinters practice
 *   polygamy as fidelity to the original revelation and claim the mainstream
 *   church apostatized rather than evolved. The constraint operates as a
 *   theological claim (polygamy remains divinely commanded) enforced through
 *   identity and institutional authority. The reading is contested by
 *   substitutionist and coercion-visibility readings that assert either the
 *   command has been replaced or its validity always depended on
 *   institutional framing.
 *
 * KEY AGENTS:
 *   - Fundamentalist continuationist movement: claims theological legitimacy for polygamy; practices it as fidelity to original revelation
 *   - Mainstream institutional church: enforces monogamy policy; frames it as self-imposed reform
 *   - Polygamy practitioners: face criminal liability; trapped between theological conviction and federal law
 *   - Women in polygamous arrangements: bear specific costs; largely excluded from the theological adjudication
 *   - Federal legal authority: exerts external pressure that forces the constraint's institutional expression
 *   - Fundamentalist splinter communities: operate in liminal legal space; practice polygamy as marker of continuity
 *   - Theological purists: provide intellectual resources to the continuationist reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, 0.68).
domain_priors:suppression_score(divine_marriage_command__continuationist_reading, 0.72).
domain_priors:theater_ratio(divine_marriage_command__continuationist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__continuationist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__continuationist_reading, "Divine Marriage Command (Continuationist Reading: Polygamy Theologically Valid, Manifesto as Prudential Suspension)").
narrative_ontology:topic_domain(divine_marriage_command__continuationist_reading, "religious/political theology").

domain_priors:requires_active_enforcement(divine_marriage_command__continuationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__continuationist_reading, '9f79be97-8db7-4f40-8da6-5bfd1d3382f6').
narrative_ontology:cs_kernel_codification('9f79be97-8db7-4f40-8da6-5bfd1d3382f6', fixed_text).
narrative_ontology:cs_authority_grounding('9f79be97-8db7-4f40-8da6-5bfd1d3382f6', extraction).
narrative_ontology:cs_interpretation_layer_present('9f79be97-8db7-4f40-8da6-5bfd1d3382f6').
narrative_ontology:cs_reading_relation('9f79be97-8db7-4f40-8da6-5bfd1d3382f6', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_reading_relation('9f79be97-8db7-4f40-8da6-5bfd1d3382f6', divine_marriage_command__coercion_visibility_reading, coexists_with).
narrative_ontology:cs_axiom('9f79be97-8db7-4f40-8da6-5bfd1d3382f6', foundational, original_revelation_immutable_despite_suspension).
narrative_ontology:cs_axiom_status(original_revelation_immutable_despite_suspension, holdable).
narrative_ontology:cs_axiom_grounding('9f79be97-8db7-4f40-8da6-5bfd1d3382f6', original_revelation_immutable_despite_suspension, deontological).
narrative_ontology:cs_axiom('9f79be97-8db7-4f40-8da6-5bfd1d3382f6', foundational, coercion_does_not_alter_doctrine).
narrative_ontology:cs_axiom_status(coercion_does_not_alter_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('9f79be97-8db7-4f40-8da6-5bfd1d3382f6', coercion_does_not_alter_doctrine, deontological).
narrative_ontology:cs_reference_frame('9f79be97-8db7-4f40-8da6-5bfd1d3382f6', pre_manifesto_doctrine).
narrative_ontology:cs_drift_state('9f79be97-8db7-4f40-8da6-5bfd1d3382f6', contemporary_polygamy_prohibition_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('9f79be97-8db7-4f40-8da6-5bfd1d3382f6', '2026-06-12T14:30:00Z').
narrative_ontology:cs_kernel_id(divine_marriage_command__continuationist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, fundamentalist_continuationist_movement).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, theological_purists).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, polygamy_practitioners_under_federal_jurisdiction).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, women_in_polygamous_arrangements).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, mainstream_institutional_church).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, fundamentalist_splinter_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the 1890 Manifesto as external accommodation to federal force, not as doctrinal rescission. Maintains that polygamy remains divinely commanded and that fundamentalist practice preserves the original revelation. Benefits from this reading by retaining theological legitimacy for the practice and by positioning mainstream institutional abandonment as apostasy rather than evolution. Collects authority within the splinter communities by holding the 'true' interpretation.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, fundamentalist_continuationist_movement, beneficiary,
    organized, civilizational, identity_locked, national).

% Officially promulgated the 1890 Manifesto and enforces discontinuation of polygamy within its institutional structure. Bears the cost of this policy in theological integrity (carrying a reading it does not fully endorse), organizational fragmentation (loss of continuationist splinters), and ongoing internal contestation. Constrained by federal law and political necessity but frames this constraint as self-imposed reform rather than external coercion.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, mainstream_institutional_church, agenda_setter,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__continuationist_reading, mainstream_institutional_church, payer).

% Face criminal liability under federal law for practicing what they believe is divinely commanded. Their theological position (continuationist reading) has no legal standing; they must either abandon the practice, relocate to undercover arrangements, or accept prosecution. The constraint extracts their religious freedom and binds them between irreconcilable duties.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, polygamy_practitioners_under_federal_jurisdiction, payer,
    powerless, biographical, trapped, national).

% Bear costs specific to polygamous structure (legal invisibility of non-first marriages, reduced economic security, social stigma, exit barriers fused with religious identity). Their voices are largely absent from the theological reading itself — the constraint is adjudicated by male theological authorities and federal law makers. The continuationist reading treats polygamy as theologically valid but does not resolve the distributional asymmetries the practice creates.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, women_in_polygamous_arrangements, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__continuationist_reading, women_in_polygamous_arrangements, excluded).

% Enforces federal law criminalizing polygamy. Exerts external pressure that forces the mainstream church's 1890 Manifesto and creates the conditions under which continuationist splinters emerge as resistance formations. Does not adjudicate the theological reading itself but creates the structural constraint the reading exists to navigate.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, federal_legal_authority, agenda_setter,
    institutional, generational, analytical, national).

% Claim institutional and theological continuity with the pre-1890 church. Practice polygamy in deliberate violation of federal law as a marker of fidelity to original revelation. Benefit from the continuationist reading by claiming they preserve the true doctrine while the mainstream church compromised. Operate in liminal legal space, tolerated in some jurisdictions, prosecuted in others.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, fundamentalist_splinter_communities, beneficiary,
    moderate, civilizational, constrained, regional).

% Academic and interpretive communities (religionists, theologians, historians) who maintain the continuationist reading as internally coherent — that the Manifesto is prudential, not doctrinal. Benefit from this reading by defending it as a defensible theological position and by providing intellectual resources to fundamentalist communities. Occupy an observer role but provide legitimacy through scholarship.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, theological_purists, beneficiary,
    analytical, civilizational, analytical, global).

% Largely absent from the theological reading itself. Bound by institutional policy against polygamy but their own theological views on whether the Manifesto is doctrinal or prudential are not determinative of official position. Experience the constraint as institutional rule they must follow regardless of their personal reading of revelation.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, mainstream_lay_membership, excluded,
    powerless, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__continuationist_reading, fundamentalist_continuationist_movement).
narrative_ontology:fixing_cost_class(divine_marriage_command__continuationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for adjudicating the theological status of polygamy after federal prohibition: if the original divine command remains in effect, how does the institutional cessation cohere with the doctrine of continuing revelation? The continuationist reading solves this by asserting that the Manifesto is accommodation to external duress, not doctrinal change, preserving the theological coherence of those who believe the command persists.
% TRANSFER_FUNCTION: Transfers authority and legitimacy: from the mainstream institutional church (which must now defend why it abandoned a practice it teaches was divinely commanded) to fundamentalist splinters (which claim they alone preserve fidelity to the original revelation). Also transfers reproductive and relational autonomy from women in polygamous arrangements to male family heads and community authorities.
% ABSENT_VOICES: Women in polygamous arrangements are largely excluded from the theological adjudication itself — the reading is debated by male authorities and lawyers, not by the women whose lives are structured by plural marriage. Mainstream lay members' own theological readings are also excluded; institutional authority forecloses internal debate on whether the Manifesto is doctrinal.
% DISAPPEARANCE_RATIONALE: If this constraint (the theological reading that polygamy remains divinely commanded) disappeared — if the mainstream church and legal authorities jointly affirmed the continuationist reading — the institutional church would fragment further, polygamous practice would shift from clandestine to defended-as-faithful, federal law enforcement would face cognitive dissonance between legal prohibition and officially endorsed theological position. The splinter communities would claim vindication. Conversely, if the reading vanishes (suppressed or definitively replaced by substitutionist doctrine), continuationist communities lose their claim to theological legitimacy and face pure legal/institutional pressure to abandon the practice.
% FOUNDING_PROBLEM: Joseph Smith taught that polygamy is divinely commanded; the 1890 Manifesto suspended the practice citing federal pressure. The founding problem: does suspending a divinely commanded practice alter its divine status, or does the command remain in effect and the Manifesto represent only institutional accommodation?
% FOUNDING_PROBLEM_CORROBORATION: Fundamentalist splinter communities, academic scholars of Mormonism (religionists and historians outside the institutional church), and theological preservationists attest the problem is live and unresolved. The mainstream institutional church's official position treats the problem as resolved (monogamy is now doctrine) but this very treatment is contested — the continuationist reading exists because that consensus is not universal. No one outside the benefiting parties (splinters, theological purists) attests the continuationist reading itself, but scholars document that it remains a live interpretive position within the tradition.
narrative_ontology:disappearance_verdict(divine_marriage_command__continuationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__continuationist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__continuationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_marriage_command__continuationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__continuationist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__continuationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__continuationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is high because the continuationist reading extracts from those it governs — women through reproductive authority concentrated in male heads; mainstream members through ideological compression (must publicly endorse monogamy while the reading asserts it is not truly doctrine); practitioners through legal vulnerability. Suppression (0.72) is substantial because the reading persists not because its proponents have overwhelming power, but because it is woven into identity and institutional authority; practitioners are suppressed by both federal law and mainstream institutional rejection. Theater ratio (0.58) reflects a hybrid: the reading has genuine theological content (the Manifesto's claim to be prudential is a coherent interpretive position), but a growing share of its persistence depends on performative maintenance of continuationist identity in splinter communities rather than on the mainstream institution's assent. The interval runs 1890–2026 to capture the long drift from the Manifesto's issuance through contemporary polarization. Measurements are authored at six time points spanning the full interval, with all three metrics sharing the same time grid.
 *
 * PERSPECTIVAL GAP:
 *   From the fundamentalist continuationist seat, this reading is liberation — it preserves doctrinal truth against institutional apostasy. From the mainstream institutional seat, it is a destabilizing claim that undermines the coherence of institutional policy. From the practitioner seats, it is constraint — the reading itself does not grant them exit from federal law, though it does provide theological justification for the practice. From women's seats, the reading is largely irrelevant to whether they have authority over their own lives; the theological claim about what God commanded does not resolve the structural asymmetry the practice creates. The engine computes these divergent directionalities from the declared beneficiary/victim structure and exit options; the claimed type (tangled rope) reflects that this is both genuine theological coordination (the reading solves the coherence problem of suspension-without-rescission) and asymmetric extraction (the reading benefits continuationist communities while extracting from practitioners and women).
 *
 * DIRECTIONALITY LOGIC:
 *   The fundamentalist continuationist movement occupies the beneficiary seat: it benefits from this reading by retaining theological legitimacy for the practice and by positioning splinters as the true preservers of doctrine. The mainstream institutional church sits dual: agenda-setter (it enforces monogamy policy and frames the reading) but also payer (it bears the cost of fragmentation and internal contestation). Practitioners and women in polygamous arrangements occupy the target seats: they pay through legal vulnerability, reproductive authority concentration, and identity-loyalty compression. Federal authority exerts external directionality — it sets the structural condition (prohibition) but does not itself adjudicate the theological reading. The spatial scope (national) amplifies the effectiveness of enforcement because federal law reaches everywhere the practitioners are. The time horizon (civilizational) for fundamentalists reflects that this reading is not tactical but claims to preserve eternal truth; for practitioners it is biographical because their lives are bounded by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint carries a clear mandate: preserve the theological claim that the original divine command remains in effect. The mandate is NOT obsolete — continuationist communities actively maintain it, splinter organizations are born from it, scholarship defends it. However, the gap between mandate and function is growing: the mainstream institution has officially abandoned the reading, federal law prohibits the practice entirely, and the reading persists primarily through identity-loyalty in communities that are structurally marginalized. This is not yet a piton (the reading has not fully atrophied into performance) but it is drifting toward one. The measurement of theater_ratio rising from 0.35 to 0.58 over the interval tracks exactly this drift: the share of continuationist activity that is performative identity maintenance (rather than substantive theological adjudication) is increasing. A mandatrophy resolution would be triggered if either: the mainstream institution definitively endorsed the substitutionist reading (which would make continuationism doctrinal apostasy), or federal law changed to permit polygamy (which would remove the coercive pressure and expose how much of the reading's persistence depends on defiant identity). Neither has happened, so the mandate remains live but increasingly contingent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_vs_prudential_boundary,
    'What is the structural difference between a prudential suspension of a doctrine and a doctrinal rescission? Can a practice be divinely commanded yet prudentially suspended indefinitely?',
    'Theological adjudication by the tradition''s own authorities (which has not produced consensus); comparison with other cases where the tradition distinguishes prudential from doctrinal change; empirical study of whether continuationist practice matches the pre-1890 form or is modified to accommodate legal risk.',
    'If the boundary is clear and the Manifesto crosses it, the substitutionist reading prevails and the continuationist reading becomes incoherent. If the boundary is porous or the Manifesto is genuinely on the prudential side, the continuationist reading remains defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_vs_prudential_boundary, conceptual, 'Whether a doctrine can be authentically suspended while remaining doctrinally valid.').

omega_variable(
    institutional_authority_vs_theological_truth,
    'Does institutional rejection of the continuationist reading (the mainstream church''s official position) constitute refutation of the reading, or merely institutional policy that can diverge from theological truth?',
    'Theological authority structures within the tradition: does authority reside in institutional hierarchy, in scriptural precedent, in accumulated interpretation, or in some combination? Different resolutions yield different verdicts on whether institutional silence/rejection has epistemic weight.',
    'If institutional authority is determinative, the continuationist reading is at best schismatic (not doctrine). If scriptural and precedent-based authority is decisive, the reading might be theologically sound despite institutional rejection. This is the axis that generates splinter movements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_vs_theological_truth, conceptual, 'Whether theological truth is determined by institutional hierarchy or by prior scriptural/interpretive authority.').

omega_variable(
    women_s_exclusion_from_adjudication,
    'Is the structural exclusion of women from the theological reading that governs their lives (polygamy''s validity) itself a feature of the reading or an artifact of authority distribution?',
    'Empirical: if women in polygamous arrangements were brought into the theological adjudication as equal voices, would the continuationist reading be endorsed, modified, or rejected? Methodologically: what happens when excluded seats participate?',
    'If women''s participation produces a different reading (e.g., convergence toward substitutionist or coercion-visibility readings), the continuationist reading''s current authority depends on exclusion — it is not a consensus but a consensus among some. This would elevate the absent_voices assessment and potentially shift the typology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(women_s_exclusion_from_adjudication, empirical, 'Whether the continuationist reading would survive inclusion of excluded voices.').

omega_variable(
    external_coercion_vs_internal_revelation,
    'Can federal law—an external, secular constraint—ever be properly distinguished from divine command—an internal, transcendent constraint? Or does the continuationist reading''s framing of the Manifesto as prudential suspension rely on a false epistemological boundary?',
    'Theological: study of how the tradition has historically handled tensions between revelation and political necessity (prior persecution, historical exigencies). Philosophical: assess whether the distinction between external coercion and internal duty is coherent.',
    'If the boundary cannot be maintained (coercion is always entangled with revelation), the continuationist reading''s core move (Manifesto is external accommodation, not internal change) becomes indefensible and the coercion_visibility reading gains ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_coercion_vs_internal_revelation, conceptual, 'Whether external coercion can be cleanly separated from internal theological authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__continuationist_reading, 1890, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__continuationist_reading, theater_ratio, 1890, 0.35).
narrative_ontology:measurement(divi_tr_t1920, divine_marriage_command__continuationist_reading, theater_ratio, 1920, 0.42).
narrative_ontology:measurement(divi_tr_t1950, divine_marriage_command__continuationist_reading, theater_ratio, 1950, 0.48).
narrative_ontology:measurement(divi_tr_t1980, divine_marriage_command__continuationist_reading, theater_ratio, 1980, 0.54).
narrative_ontology:measurement(divi_tr_t2000, divine_marriage_command__continuationist_reading, theater_ratio, 2000, 0.56).
narrative_ontology:measurement(divi_tr_t2026, divine_marriage_command__continuationist_reading, theater_ratio, 2026, 0.58).

% Extraction over time
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__continuationist_reading, base_extractiveness, 1890, 0.48).
narrative_ontology:measurement(divi_be_t1920, divine_marriage_command__continuationist_reading, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(divi_be_t1950, divine_marriage_command__continuationist_reading, base_extractiveness, 1950, 0.62).
narrative_ontology:measurement(divi_be_t1980, divine_marriage_command__continuationist_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(divi_be_t2000, divine_marriage_command__continuationist_reading, base_extractiveness, 2000, 0.67).
narrative_ontology:measurement(divi_be_t2026, divine_marriage_command__continuationist_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__continuationist_reading, suppression_requirement, 1890, 0.55).
narrative_ontology:measurement(divi_su_t1920, divine_marriage_command__continuationist_reading, suppression_requirement, 1920, 0.62).
narrative_ontology:measurement(divi_su_t1950, divine_marriage_command__continuationist_reading, suppression_requirement, 1950, 0.67).
narrative_ontology:measurement(divi_su_t1980, divine_marriage_command__continuationist_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(divi_su_t2000, divine_marriage_command__continuationist_reading, suppression_requirement, 2000, 0.71).
narrative_ontology:measurement(divi_su_t2026, divine_marriage_command__continuationist_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__continuationist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__continuationist_reading, 0.12).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__substitutionist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% The divine_marriage_command kernel admits three distinct readings that instantiate different constraints with different beneficiary/victim structures and different measured extractiveness. The continuationist reading (this story) holds that polygamy remains valid and the Manifesto is prudential. The substitutionist reading claims the Manifesto is new revelation and monogamy is now required. The coercion_visibility reading acknowledges federal coercion as the legitimacy ground for the Manifesto. All three share the same referent (the validity/status of polygamy after 1890) but instantiate different ε values and different causal stories. They are linked by affects_constraints to enable the engine to propagate changes across the kernel's interpretive space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_marriage_command__continuationist_reading, powerless, 0.85).
constraint_indexing:directionality_override(divine_marriage_command__continuationist_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
