% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__hybrid_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality_hybrid_continuity_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: hebrew_vitality__hybrid_continuity_reading
 *   human_readable: Hebrew Vitality: Hybrid Continuity Reading (Liturgical + Vernacular)
 *   domain: sociolinguistics/language_revitalization
 *
 * SUMMARY:
 *   This constraint instantiates the HYBRID CONTINUITY READING of the Hebrew
 *   vitality kernel — the proposal that liturgical preservation and
 *   vernacular revival are complementary rather than competitive, and that
 *   language vitality requires BOTH substrate (maintained through ritual and
 *   textual tradition) and living practice (native generation, daily use,
 *   innovation). The reading emerges from academic revitalization studies and
 *   attempts to resolve a theoretical and policy deadlock between
 *   preservationists (who saw liturgy as sufficient) and modernists (who saw
 *   vernacular generation as the only 'real' vitality). The constraint's ε is
 *   deliberately low (0.15): this is an analytical reframing, a proposed
 *   resolution of a contested binary, not itself an actionable extraction
 *   mechanism. It carries no clear beneficiary/victim structure — the
 *   beneficiary is the academic community and policy makers who gain a
 *   synthesis framework, and there is no identifiable target bearing costs
 *   from the reframing itself. The authored claim is ROPE (coordination of
 *   resources and institutional frameworks around a unified model); the
 *   metrics reflect low extractiveness because the constraint does not
 *   extract material rents, only institutional attention and resource
 *   allocation ratios.
 *
 * KEY AGENTS:
 *   - academic_revitalization_community: institutional beneficiary and corroborating observer — supplies the synthesis framework and empirical case studies
 *   - liturgical_tradition_keepers: organized beneficiary with civilizational horizon — validated as foundational; practice recognized as essential substrate
 *   - native_speaker_communities: powerful beneficiary/payer (mobile, generational horizon) — gain institutional recognition of vernacular vitality but also bear costs of engaging with preservation frameworks
 *   - language_policy_makers: institutional agenda-setter — must allocate resources between channels; this reading justifies dual-track investment
 *   - liturgical_skeptics: excluded (moderate power) — would deny that ritual constitutes vitality; barred from this consensus
 *   - vernacular_purists: excluded (moderate power) — would deny that antiquarian substrate is necessary; barred from this consensus
 *   - comparative_linguists: analytical observers — can provide external evidence on whether hybrid pathways work in other revitalization cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__hybrid_continuity_reading, 0.15).
domain_priors:suppression_score(hebrew_vitality__hybrid_continuity_reading, 0.08).
domain_priors:theater_ratio(hebrew_vitality__hybrid_continuity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_vitality__hybrid_continuity_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__hybrid_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__hybrid_continuity_reading, "Hebrew Vitality: Hybrid Continuity Reading (Liturgical + Vernacular)").
narrative_ontology:topic_domain(hebrew_vitality__hybrid_continuity_reading, "sociolinguistics/language_revitalization").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__hybrid_continuity_reading, '52950bb6-2746-419d-813a-dc4a1df316ae').
narrative_ontology:cs_kernel_codification('52950bb6-2746-419d-813a-dc4a1df316ae', distributed).
narrative_ontology:cs_authority_grounding('52950bb6-2746-419d-813a-dc4a1df316ae', distributed).
narrative_ontology:cs_reading_relation('52950bb6-2746-419d-813a-dc4a1df316ae', hebrew_vitality__liturgical_reading, coexists_with).
narrative_ontology:cs_reading_relation('52950bb6-2746-419d-813a-dc4a1df316ae', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_axiom('52950bb6-2746-419d-813a-dc4a1df316ae', foundational, vitality_is_multidimensional).
narrative_ontology:cs_axiom_status(vitality_is_multidimensional, holdable).
narrative_ontology:cs_axiom_grounding('52950bb6-2746-419d-813a-dc4a1df316ae', vitality_is_multidimensional, empirically_contingent).
narrative_ontology:cs_axiom('52950bb6-2746-419d-813a-dc4a1df316ae', foundational, substrate_and_usage_are_complementary).
narrative_ontology:cs_axiom_status(substrate_and_usage_are_complementary, holdable).
narrative_ontology:cs_axiom_grounding('52950bb6-2746-419d-813a-dc4a1df316ae', substrate_and_usage_are_complementary, empirically_contingent).
narrative_ontology:cs_reference_frame('52950bb6-2746-419d-813a-dc4a1df316ae', unified_revitalization_model).
narrative_ontology:cs_drift_state('52950bb6-2746-419d-813a-dc4a1df316ae', contemporary_institutional_adoption, gap(stable, minor, true)).
narrative_ontology:cs_created_at('52950bb6-2746-419d-813a-dc4a1df316ae', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, academic_revitalization_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, liturgical_tradition_keepers).
narrative_ontology:constraint_beneficiary(hebrew_vitality__hybrid_continuity_reading, native_speaker_communities).
narrative_ontology:constraint_victim(hebrew_vitality__hybrid_continuity_reading, native_speaker_communities).
narrative_ontology:constraint_vindicates(hebrew_vitality__hybrid_continuity_reading, language_vitality_is_multidimensional).
narrative_ontology:constraint_vindicates(hebrew_vitality__hybrid_continuity_reading, liturgical_preservation_and_native_generation_are_complementary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develops and teaches revitalization methodologies integrating liturgical substrate and vernacular generation. Benefits from a framing that reconciles the liturgical/native divide and justifies sustained investment in both channels. Provides expert corroboration for why both pathways matter and how they interact.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, academic_revitalization_community, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__hybrid_continuity_reading, academic_revitalization_community, observer).

% Maintain unbroken liturgical Hebrew practice across centuries. This reading acknowledges liturgical preservation as a necessary enabler — it provides the substrate that vernacular revival builds on. Their practice is vindicated as foundational, not merely decorative or backward-looking.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, liturgical_tradition_keepers, beneficiary,
    organized, civilizational, identity_locked, global).

% Live Hebrew natively in daily contexts, recreating the language generationally through use rather than preservation. This reading validates that vernacular generation is essential — liturgical substrate alone cannot sustain vitality. They also bear a cost: the constraint requires engagement with institutional frameworks (schools, policy bodies) that may not serve native speaker priorities.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, native_speaker_communities, beneficiary,
    powerful, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__hybrid_continuity_reading, native_speaker_communities, payer).

% Decide resource allocation between liturgical preservation (museums, ritual sites, textual scholarship) and vernacular education (schools, media, informal networks). This reading supplies a framework for justifying investment in both, resolving the zero-sum framing that pitted one against the other.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, language_policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% Hold that liturgical recitation is preservation only, not vitality — that it freezes the language in crystallized forms and defers real life. They would argue this hybrid reading falsely elevates ritual to co-essential status and should not drive policy. They are structurally excluded from the consensus represented by this reading.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, liturgical_skeptics, excluded,
    moderate, biographical, constrained, regional).

% Hold that only native generation constitutes vitality — that liturgical study is antiquarian fetishism. They would argue this hybrid reading legitimizes a backward-looking substrate at the expense of forward momentum. They are excluded from the consensus that this reading proposes.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, vernacular_purists, excluded,
    moderate, biographical, constrained, regional).

% Examine language revitalization across cases (Irish, Maori, Catalan, Quechua) and can report on whether hybrid pathways combining preservation and generation show better vitality outcomes than single-channel approaches. Analytical seat, external to the Hebrew-specific debate.
narrative_ontology:constraint_stakeholder(hebrew_vitality__hybrid_continuity_reading, comparative_linguists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__hybrid_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_vitality__hybrid_continuity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves a false binary between liturgical preservation and vernacular revival by establishing that both are necessary components of comprehensive vitality: liturgy provides the substrate (lexicon, grammar, texts), vernacular generation provides the living practice (usage, innovation, daily need). The reading coordinates institutional frameworks to invest in both rather than forcing a choice.
% TRANSFER_FUNCTION: Transfers legitimacy and resources from debates framed as 'preservation vs. life' to a unified model that recognizes complementarity. Academic institutions gain a synthetic framework to teach; policy makers gain justification for dual-track funding; tradition keepers gain recognition that their practice is foundational, not peripheral.
% ABSENT_VOICES: Liturgical skeptics (who deny that ritual constitutes any vitality) and vernacular purists (who deny that antiquarian substrate matters) are structurally excluded by this reading's core synthesis. They would contest whether the hybrid framing is coherent or instead smuggles in a false concession to the other side.
% DISAPPEARANCE_RATIONALE: If this hybrid reading disappeared, policy frameworks would revert to zero-sum allocations pitting preservation against education. Academic programs would fragment into separate liturgical and conversational silos. The native speaker communities would lose institutional recognition of their practice's dependence on preserved substrate, and the tradition keepers would lose recognition of vitality's dependence on native use. Resource competition would intensify, and some language communities might abandon either liturgical or vernacular initiatives under budget pressure.
% FOUNDING_PROBLEM: Hebrew revitalization presented a false binary: either language vitality consisted in unbroken liturgical use (the preservationist reading) or it required only native generation independent of ancient texts (the modernist reading). This dichotomy stalled both institutional coordination and theoretical understanding. Communities that had maintained liturgical practice but lacked native speakers, or that had acquired native speakers through deliberate education but lacked deep textual rootedness, were forced to choose which aspect 'counted.' The founding problem is that binary choice architecture itself.
% FOUNDING_PROBLEM_CORROBORATION: Historical linguists document that modern Hebrew emerged from a synthesis: liturgical texts and grammar provided the material for revival, but revival succeeded only where deliberate native-generation efforts (schools, families, street-use policy) built living practice around that material. Comparative linguists studying other revitalization cases (Irish, Catalan, Quechua) report that successful efforts typically combine resource-layer recovery (texts, grammar description) with usage-layer generation (immersion, daily-life deployment). The academic revitalization community and policy analysts outside both the liturgical and modernist camps attests that the binary framing has been empirically unproductive. The liturgical keepers and vernacular communities themselves attest differently — they remain contested on whether the hybrid reading achieves coherence or merely postpones choice.
narrative_ontology:disappearance_verdict(hebrew_vitality__hybrid_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__hybrid_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__hybrid_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_vitality__hybrid_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__hybrid_continuity_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__hybrid_continuity_reading_tests).
:- end_tests(hebrew_vitality__hybrid_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15 at interval end) because this reading operates as an analytical resolution, not as a mechanism that extracts from payers to beneficiaries. It proposes a reframing of institutional policy, not a coercive transfer. Theater ratio is moderate (0.22) because academic consensus-building includes performative elements (conferences, publications) alongside genuine analytical work. Suppression is very low (0.08) because the constraint does not depend on coercing skeptics into silence — it operates by intellectual persuasion and institutional adoption. Accessibility_collapse (0.35) is low because the alternative framings (liturgical only, vernacular only, binary choice) remain live positions held by excluded voices; the hybrid reading has not collapsed alternative interpretations entirely. Resistance (0.42) is modest because some tradition keepers and modernists actively resist the synthetic framing, seeing it as a false compromise. The measurement series tracks modest growth in extractiveness over the interval (as policy institutions adopt the framework and begin redirecting resources) but plateaus when adoption stabilizes — no mechanism is driving runaway extraction.
 *
 * PERSPECTIVAL GAP:
 *   The liturgical keepers and the native speaker communities have different relationships to this constraint. The keepers gain validation but also incur a cost: they must now defend their practice not on its own terms but as part of a larger hybrid model. The native speakers gain institutional backing but must also accept that preservationist institutions have claims on policy attention. The policy makers gain clarity but lose the freedom to choose. The excluded skeptics lose institutional standing without losing the right to their view. These are the per-seat divergences the engine should compute.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality here is muted because the constraint is not extractive in the classical sense. The academic community benefits (low d, ~0.15) by gaining a synthesis to teach and fund; the policy makers benefit by resolving ambiguity (low d, ~0.25); the tradition keepers benefit by recognition (moderate d, ~0.35 — they carry some identity-lock cost from engagement with institutional frameworks that may not honor their practice's integrity); the native speakers sit near symmetric (d ~0.50 — genuine coordination benefit from institutional investment, but also cost of engaging with preservation frameworks). The excluded voices sit high on d (~0.75–0.80) because the constraint's adoption forecloses their readings from institutional legitimacy, even though the constraint does not directly extract from them. No override is needed; the structural data yields coherent directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy. Its founding problem — the false binary that forced choice between preservation and generation — is genuinely contested and remains live. The problem has not died; the contest has simply shifted to whether the hybrid reading coherently resolves it. The constraint's mandate (coordinate resources for both pathways) remains aligned with the founding problem (avoid forced choice). The theater ratio is modest because the reframing includes real analytical work, not merely performative maintenance of a defunct function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synthetic_coherence_ambiguity,
    'Does the hybrid reading achieve genuine analytical coherence, or does it merely postpone the underlying choice between liturgical preservation and vernacular generation?',
    'Long-term empirical tracking of revitalization outcomes in communities that adopt the hybrid model: if dual-track investment produces better vitality indicators than single-channel approaches, the coherence claim is supported; if zero-sum pressures reassert and communities revert to choosing, the synthesis is pragmatic but not structurally coherent.',
    'If the hybrid reading is truly coherent, it resolves a theoretical contest and justifies sustained dual investment; if it merely defers choice, the constraint is a temporary consensus that will dissolve under budget pressure. Classification impact: genuine coherence supports rope (coordination); instability supports scaffold (temporary hold) or piton (theater masking unresolved choice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthetic_coherence_ambiguity, empirical, 'Whether the hybrid reading achieves structural coherence or is a pragmatic compromise that will decompose.').

omega_variable(
    excluded_skeptic_foreclosure,
    'Do the liturgical skeptics and vernacular purists remain genuinely excluded from the hybrid reading''s institutional space, or do they possess latent veto power that will resurface under institutional stress?',
    'Policy and institutional stability test: if budget cuts or competing priorities force real choice between preservation and generation, do the excluded skeptics gain standing to reopen the debate? If yes, the exclusion was performative; if no, it is structural.',
    'Structural exclusion supports the rope classification (stable coordination); performative exclusion suggests scaffold (temporary consensus vulnerable to external pressure). If skeptics can force a choice, the constraint is less stable than the low suppression metric suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_skeptic_foreclosure, empirical, 'Whether the excluded skeptics remain genuinely barred from policy space or retain veto power.').

omega_variable(
    kernel_contest_under_determination,
    'Is the underlying kernel contest (what constitutes vitality) actually resolvable through reframing, or is it a preference question that cannot be analytically adjudicated?',
    'If the hybrid reading is accepted as institutional standard but the sibling readings remain live in practice (communities still organize around liturgy-vs-generation choice), the contest remains preference-level; if acceptance of the hybrid reading causes the sibling readings to fade, the contest was resolvable through reframing.',
    'If preference-level, this reading is a committer-frame choice (one reading of a fundamentally contested kernel), not a resolution; classification remains stable but the contest persists. If resolvable, the reading may eventually displace siblings; the kernel''s contest closes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_under_determination, preference, 'Whether the kernel contest is analytically resolvable or a preference question.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__hybrid_continuity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t8, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement_basis(hebr_tr_t8, observed).
narrative_ontology:measurement(hebr_tr_t16, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement_basis(hebr_tr_t16, observed).
narrative_ontology:measurement(hebr_tr_t24, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement_basis(hebr_tr_t24, observed).
narrative_ontology:measurement(hebr_tr_t32, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 32, 0.22).
narrative_ontology:measurement_basis(hebr_tr_t32, projected).
narrative_ontology:measurement(hebr_tr_t40, hebrew_vitality__hybrid_continuity_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(hebr_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t8, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 8, 0.11).
narrative_ontology:measurement_basis(hebr_be_t8, observed).
narrative_ontology:measurement(hebr_be_t16, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 16, 0.13).
narrative_ontology:measurement_basis(hebr_be_t16, observed).
narrative_ontology:measurement(hebr_be_t24, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 24, 0.15).
narrative_ontology:measurement_basis(hebr_be_t24, observed).
narrative_ontology:measurement(hebr_be_t32, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 32, 0.16).
narrative_ontology:measurement_basis(hebr_be_t32, projected).
narrative_ontology:measurement(hebr_be_t40, hebrew_vitality__hybrid_continuity_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement_basis(hebr_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(hebr_su_t0, observed).
narrative_ontology:measurement(hebr_su_t8, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 8, 0.06).
narrative_ontology:measurement_basis(hebr_su_t8, observed).
narrative_ontology:measurement(hebr_su_t16, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 16, 0.07).
narrative_ontology:measurement_basis(hebr_su_t16, observed).
narrative_ontology:measurement(hebr_su_t24, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 24, 0.08).
narrative_ontology:measurement_basis(hebr_su_t24, observed).
narrative_ontology:measurement(hebr_su_t32, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 32, 0.08).
narrative_ontology:measurement_basis(hebr_su_t32, projected).
narrative_ontology:measurement(hebr_su_t40, hebrew_vitality__hybrid_continuity_reading, suppression_requirement, 40, 0.08).
narrative_ontology:measurement_basis(hebr_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__hybrid_continuity_reading, information_standard).
narrative_ontology:boltzmann_floor_override(hebrew_vitality__hybrid_continuity_reading, 0.08).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__hybrid_continuity_reading, hebrew_vitality__native_daily_reading).

% DUAL FORMULATION NOTE:
% The Hebrew vitality kernel decomposes into three distinct constraints: (1) liturgical_reading — ritual preservation as sufficient vitality, ε~0.60, type snare (suppressed alternatives, clear extraction from modernists); (2) native_daily_reading — native generation as necessary vitality, ε~0.45, type tangled_rope (real coordination of education, asymmetric extraction from tradition keepers); (3) hybrid_continuity_reading — both as complementary, ε~0.15, type rope (analytical synthesis, low extraction). The three readings coexist as live positions in the contest; no reading forecloses the others at the individual party level. The hybrid reading coexists_with both siblings and influences their resource allocation by proposing dual-track investment justification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
