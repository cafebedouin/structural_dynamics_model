% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
 *   human_readable: Prophetic Reinterpretation of Marriage Commitment (Endogenous Divine Authority Reading)
 *   domain: religious/institutional
 *
 * SUMMARY:
 *   This constraint instantiates one reading of a contested kernel: the
 *   legitimacy of the 1890 Manifesto that reversed the church's official
 *   stance on plural marriage. Under this endogenous reinterpretation
 *   reading, the Manifesto represents genuine divine revelation through
 *   prophetic succession. God commanded the reversal to preserve the church
 *   for higher theological purposes—allowing entry into US statehood,
 *   avoiding schism, and clarifying that monogamy, not plural marriage, is
 *   the covenant form for the current dispensation. Federal pressure (Utah
 *   statehood conditions, polygamy prosecutions) is the context in which this
 *   new revelation became necessary, but not its originating cause. The
 *   prophetic authority's legitimacy depends on the capacity to receive and
 *   act on new divine instruction, and the reinterpretation vindicates the
 *   doctrine of living revelation itself. This reading has substantial
 *   theological coherence internal to the tradition but faces empirical
 *   contestation from historians and sociologists who read federal pressure
 *   as the primary causal mechanism and the theological language as post-hoc
 *   justification.
 *
 * KEY AGENTS:
 *   - Prophetic Authority Lineage: The institutional seat holding authority to receive and interpret new divine revelation; collects authority consolidation and doctrinal continuity from the reinterpretation.
 *   - Faithful Adherents in Plural Marriages: Bearers of the cost of covenant evolution; required to dissolve marriages to remain in good standing; identity-locked (apostasy is the alternative exit).
 *   - Federal Government: Structural catalyst (legal and territorial pressure); under this reading, not the originating cause but the context prompting divine clarification.
 *   - Theological Tradition Continuity: The vindicated proposition that living revelation is real and functional; the constraint's operation proves the tradition's capacity to evolve while maintaining legitimacy.
 *   - Dissenting Traditionalists and Splinter Groups: Excluded from reinterpretive authority; their theological objections framed as resistance to new revelation rather than legitimate alternative readings.
 *   - Academic Historians: External observers assessing whether the theological framing is authentic or post-hoc.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.28).
domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.15).
domain_priors:theater_ratio(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "Prophetic Reinterpretation of Marriage Commitment (Endogenous Divine Authority Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "religious/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, '5442b78f-baca-43a1-99c6-def1eb10ca3a').
narrative_ontology:cs_kernel_codification('5442b78f-baca-43a1-99c6-def1eb10ca3a', formalized).
narrative_ontology:cs_authority_grounding('5442b78f-baca-43a1-99c6-def1eb10ca3a', lineage).
narrative_ontology:cs_interpretation_layer_present('5442b78f-baca-43a1-99c6-def1eb10ca3a').
narrative_ontology:cs_reading_relation('5442b78f-baca-43a1-99c6-def1eb10ca3a', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('5442b78f-baca-43a1-99c6-def1eb10ca3a', marriage_commitment_legitimacy__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('5442b78f-baca-43a1-99c6-def1eb10ca3a', foundational, prophetic_reinterpretation_is_divine_will).
narrative_ontology:cs_axiom_status(prophetic_reinterpretation_is_divine_will, holdable).
narrative_ontology:cs_axiom_grounding('5442b78f-baca-43a1-99c6-def1eb10ca3a', prophetic_reinterpretation_is_divine_will, deontological).
narrative_ontology:cs_axiom('5442b78f-baca-43a1-99c6-def1eb10ca3a', foundational, covenant_stages_evolve_under_living_revelation).
narrative_ontology:cs_axiom_status(covenant_stages_evolve_under_living_revelation, holdable).
narrative_ontology:cs_axiom_grounding('5442b78f-baca-43a1-99c6-def1eb10ca3a', covenant_stages_evolve_under_living_revelation, theological).
narrative_ontology:cs_axiom('5442b78f-baca-43a1-99c6-def1eb10ca3a', secondary, federal_pressure_is_catalyst_not_cause).
narrative_ontology:cs_axiom_status(federal_pressure_is_catalyst_not_cause, holdable).
narrative_ontology:cs_axiom_grounding('5442b78f-baca-43a1-99c6-def1eb10ca3a', federal_pressure_is_catalyst_not_cause, conventional).
narrative_ontology:cs_reference_frame('5442b78f-baca-43a1-99c6-def1eb10ca3a', living_revelation_doctrine_prophetic_succession).
narrative_ontology:cs_drift_state('5442b78f-baca-43a1-99c6-def1eb10ca3a', contemporary_post_federal_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5442b78f-baca-43a1-99c6-def1eb10ca3a', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_authority_lineage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, faithful_adherents_polygamous_marriages).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The church's prophetic succession holds authority to reinterpret doctrine in response to divine revelation. Under this reading, the leadership receives divine communication that monogamy, not plural marriage, represents God's will for the current covenant stage. The authority's legitimacy—and continuity with founding revelation—depends on the capacity to receive and act on new divine instruction. The Manifesto is authored as a statement of this new revelation, not as capitulation to external force.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_authority_lineage, agenda_setter,
    institutional, civilizational, trapped, global).

% Members in plural marriages at the time of the Manifesto are required to dissolve all but the first marriage to remain in good standing. This reading frames the cost not as persecution but as obedience to divine reorientation. Exit means apostasy—severance from the only spiritual framework that has constituted their identity and community. The framing preserves their dignity by treating them as participants in covenant evolution, not victims of institutional betrayal.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, faithful_adherents_polygamous_marriages, payer,
    powerless, biographical, identity_locked, regional).

% Applied legal and territorial pressure (Utah statehood conditions, polygamy prosecutions) that prompted the institutional crisis. Under this reading, federal pressure is the catalyst for divine clarification—the context in which God's new revelation becomes necessary—but not its cause. The government's role is structural background, not the originating force.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, federal_government, observer,
    institutional, generational, analytical, national).

% Members who believe plural marriage was divinely mandated and cannot be revoked. They are excluded from the reinterpretive process—their theological objections are treated as resistance to new revelation rather than as legitimate alternative prophetic readings. Some remain in the church but in formal dissent; others separate entirely.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, dissenting_traditionalists, excluded,
    moderate, biographical, trapped, regional).

% Splinter groups that reject the Manifesto and maintain polygamous practice as divinely mandated. They claim the prophetic authority was overridden by external pressure and no genuine revelation occurred. Their theological objections and lived practice are incompatible with both this reading and institutional continuity.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, rival_polygamous_traditions, excluded,
    powerless, biographical, trapped, regional).

% External scholarly observers who examine the documentary record, timing, and causal structure. They assess whether the Manifesto's framing as divine revelation is consistent with or contradicted by the historical evidence of federal pressure, internal institutional deliberation, and the degree to which the language tracks divine versus pragmatic reasoning.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, academic_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_authority_lineage).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Legitimates the reinterpretation of a foundational practice (plural marriage) as compatible with ongoing covenant development and prophetic authority. The coordination problem solved: how can a tradition claiming living revelation modify core doctrines without discrediting its claims to divine guidance? This reading answers: through the prophetic mechanism itself—new revelation clarifies God's will for a new stage.
% TRANSFER_FUNCTION: Transfers authority to reinterpret doctrine from scriptural text (treated as fixed) to prophetic succession (treated as dynamic). Also transfers the burden of covenant obedience from plural marriage to monogamy, from those already married to new generations entering marriage. The cost is borne by adherents with existing plural marriages and by traditionalists whose theology is displaced.
% ABSENT_VOICES: Rival polygamous traditions that reject the prophetic authority's legitimacy; dissenting traditionalists who believe plural marriage cannot be revoked; polygamous wives whose voice in the dissolution decision is structurally absent (obedience is framed as individual, not household, choice); non-adherent women whose plural-marriage dissolution affects their security and standing.
% DISAPPEARANCE_RATIONALE: If this reinterpretive reading vanished—if the institutional claim to prophetic authority over doctrine were abandoned—the tradition would fracture into incompatible factions (those accepting plural marriage as eternal doctrine, those accepting monogamy as current practice). The institutional coherence and theological legitimacy the reading provides would be lost; the tradition would splinter or dissolve.
% FOUNDING_PROBLEM: A foundational practice (plural marriage) came into conflict with the legal and territorial conditions of US statehood. The institution faced a choice: modify the practice, maintain it and suffer legal consequences, or separate from the federal system entirely. The theological reading locates the problem in God's evolving will for covenant stages, not in institutional pressure.
% FOUNDING_PROBLEM_CORROBORATION: The prophetic leadership and faithful believers attest that God's will evolved and the revelation is genuine. Federal authorities, contemporary observers, and modern historians attest that federal pressure was the proximate cause and theological reframing was the accommodation strategy. No external theological authority (other mainstream Christian denominations, academic religious scholars) corroborates the claim that the Manifesto represents authentic divine revelation rather than institutional adaptation. The corroboration question is precisely what the omega variables address.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).
:- end_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) under this reading because the beneficiary is an abstract authority structure (prophetic succession), not a concrete actor. No individual or group visibly collects concentrated rents from the reinterpretation—the church as a whole faces the pressure to adapt, not a faction extracting from the rest. Suppression is very low (0.15) because obedience is framed as voluntary reception of divine will, not coerced compliance. Resistance is present (0.38) but is treated as spiritual resistance to new revelation, not as structural resistance to institutional extraction. Theater ratio is minimal (0.12) because under this reading, the theological substance (reinterpretation as covenant evolution) is genuine—the performance is minimal. The measurement trajectory shows base_extractiveness rising slightly from 1880 to 1900 as the institutional mechanism consolidates (enforcement of the Manifesto's requirements on existing plural marriages), then declining slightly by 1910 as the policy stabilizes and obedience becomes normalized. This is consistent with a rope reading: initial coordination need (the reinterpretation) driving modest institutional enforcement, then settling into cultural practice. The claim-metric gap (claimed rope, low extractiveness metrics) is intentional: the engine decides whether these structural values sustain the rope classification or suggest a different type.
 *
 * PERSPECTIVAL GAP:
 *   From the prophetic authority's seat, the reinterpretation is a triumph: the tradition demonstrates living revelation in action, adapts without schism, preserves institutional coherence. From the seat of a faithful adherent in a plural marriage, the reinterpretation is experienced as constraint reorientation: the ground of religious identity shifts. From the federal government's seat, the reinterpretation is a capitulation dressed in theological language. From the historian's seat, the reinterpretation is empirically contestable—the timing, the language, and the outcome align with federal pressure more than with spontaneous divine clarification. The engine computes these divergent classifications (per-seat types) from the structural data; the reading ensures all seats are described from within a single interpretive framework (divine authority reorienting doctrine).
 *
 * DIRECTIONALITY LOGIC:
 *   The prophetic authority lineage sits at the beneficiary end of the directionality spectrum (d ≈ 0.15): it collects authority consolidation, doctrinal control, and institutional coherence from the reinterpretation. Faithful adherents in plural marriages sit near the target end (d ≈ 0.85): they bear the cost of obedience—dissolution of marriages, identity reconstruction—without having authored the reinterpretation or controlled its theological frame. However, under this reading, both are framed as participants in covenant evolution, not as extractor and victim. The federal government sits at analytical distance (d = 0.5): it applies external pressure but is not structurally positioned within the constraint itself. Dissenting traditionalists and splinter groups are partially trapped by the reinterpretation: their alternative theological reading (plural marriage as eternal) becomes institutionally illegitimate, yet exit (joining rival groups) means religious rupture. The key structural asymmetry: the prophetic authority controls the interpretive frame (authority_grounding: lineage, kernel_codification: fixed_text mediating toward new revelation), which privileges its reading over alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (conflict between plural marriage doctrine and US territorial/legal conditions) is classified as 'contested' rather than 'dead' because traditionalists argue the problem persists: plural marriage remains doctrinally correct even if institutionally suspended. The reinterpretation vindicates the doctrine of living revelation (vindicated_propositions: living_revelation_doctrine, prophetic_succession_legitimacy), which both protects the tradition from mandatrophy (the institution demonstrates it can evolve under divine guidance) and makes the claim contestable. If the founding problem is genuinely solved (God's will has evolved; the conflict is resolved), the reinterpretation is not mandatrophic—it represents successful adaptation. If the founding problem is obscured rather than solved (federal pressure forced the reversal, theology rationalized it), the constraint approaches mandatrophy (the institution maintains a reinterpretation story while the real coordinating problem—conflict with federal authority—persists unresolved). The omega variables address this ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manifesto_authenticity_reading_vs_cause,
    'Does the Manifesto represent genuine divine revelation received through prophetic succession, or does it represent institutional rationalization of federal pressure?',
    'Archival analysis of prophetic leadership''s private correspondence, deliberation records, and theological reasoning contemporaneous with the Manifesto''s composition. Comparative analysis of other prophetic reversals and their relationship to external institutional pressure. Interview data from participants or their descendants about the experienced frame (revelation vs. necessity).',
    'If the Manifesto is authentic divine revelation, this reading holds; extractiveness remains low (authority structure benefits, adherents obey divine will). If the Manifesto is post-hoc rationalization, the reading dissolves into the exogenous_override_reading; extractiveness rises sharply (authority structure extracts institutional legitimacy through theological cover; adherents are coerced under a false frame).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(manifesto_authenticity_reading_vs_cause, empirical, 'Whether the Manifesto''s claim to divine revelation is historically authentic or a constructed cover for federal coercion.').

omega_variable(
    prophetic_authority_legitimacy_framework_dependence,
    'Does the legitimacy of the prophetic reinterpretation depend on the doctrine of living revelation being true, or can it be assessed independent of that theological commitment?',
    'Engagement with the tradition''s own epistemological claims about prophetic succession. Examination of how the tradition would respond if empirical evidence contradicted the prophetic claim (e.g., if documents proved the Manifesto was authored under external duress). Assessment of whether the tradition treats prophetic claims as subject to empirical testing or as protected from external falsification.',
    'If living revelation is an unfalsifiable doctrine (protected by the tradition''s epistemology), then the reading is self-sustaining—no external evidence can dislodge it, and it functions as a closed interpretive frame. If living revelation is subject to empirical testing, then contradictory historical evidence (federal pressure as primary cause) could invalidate the reading. This determines whether the constraint is a genuine rope (coordination function is real) or a snare (extracted authority is protected by epistemological immunity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prophetic_authority_legitimacy_framework_dependence, conceptual, 'Whether the reading''s legitimacy depends on commitment to living revelation doctrine, or can be assessed on historical-causal grounds alone.').

omega_variable(
    identity_lock_vs_voluntary_obedience,
    'Is the obedience of faithful adherents to the Manifesto''s requirement (dissolution of plural marriages) experienced as voluntary covenant realignment, or as forced compliance rationalized through theological language?',
    'Qualitative analysis of conversion narratives, testimony records, and personal accounts from adherents describing their experience of the Manifesto. Analysis of variation in obedience rates and resistance patterns across different demographic groups and regions. Examination of institutional pressure mechanisms (testimony, social sanction, ecclesiastical consequences) deployed to enforce the Manifesto.',
    'If experienced as voluntary covenant realignment, the exit_options for adherents are accurately coded as identity_locked but not suppressed—obedience is genuine. If experienced as forced compliance, suppression rises sharply, and the constraint moves toward snare classification (coerced acceptance of institutional reframing, not authentic reorientation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_voluntary_obedience, empirical, 'Whether adherent obedience to the Manifesto is experienced as authentic theological realignment or as forced compliance under institutional pressure.').

omega_variable(
    committer_frame_kernel_alternative_readings,
    'How does this endogenous_reinterpretation_reading relate structurally to its sibling readings (exogenous_override_reading, hybrid_pragmatic_reading)? Which reading best accounts for the historical and theological evidence?',
    'Detailed comparison of the three readings'' fit to: (a) timing of institutional deliberation vs. federal pressure escalation, (b) theological language used in the Manifesto vs. language in other prophetic statements, (c) institutional consequences (consolidation of authority, schism prevention, covenant evolution), (d) contemporary and dissenting accounts from within and outside the tradition, (e) the Manifesto''s own framing (does it claim divine revelation or pragmatic necessity?).',
    'This omega documents that the three readings are competing instantiations of the same kernel, and that assessing the endogenous_reinterpretation_reading requires explicit comparison to its siblings. No reading can be evaluated in isolation from the alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_kernel_alternative_readings, conceptual, 'The committer-frame ambiguity: the Manifesto''s legitimacy is contested between three coherent readings of the same kernel.').

omega_variable(
    suppression_of_dissenting_traditionalists,
    'Does the institutional suppression of dissenting traditionalists (who reject the reinterpretation and maintain plural marriage as divinely mandated) constitute part of the constraint''s suppression metric, or is it external to the constraint structure?',
    'Analysis of institutional mechanisms used to marginalize, exclude, or pressure dissenting voices: formal doctrinal pronouncements against plural marriage, ecclesiastical sanctions on traditionalists, organizational mechanisms that prevent dissenting theology from being taught or transmitted, rhetorical framing that treats dissent as spiritual resistance rather than legitimate theological alternative.',
    'If dissenting suppression is internal to the constraint, base_properties.suppression should be higher (0.25-0.35 rather than 0.15). If dissenting suppression is external institutional politics rather than part of the constraint''s operation, the current suppression score holds. This determines whether the constraint is a rope (low suppression, genuine coordination) or a tangled_rope (suppression of alternatives required to maintain the reinterpretation''s hegemony).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_dissenting_traditionalists, empirical, 'Whether institutional suppression of dissenting traditionalists is internal or external to the constraint''s suppression metric.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 1880, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1880, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1880, 0.08).
narrative_ontology:measurement(marr_tr_t1887, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1887, 0.1).
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.12).
narrative_ontology:measurement(marr_tr_t1900, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1900, 0.13).
narrative_ontology:measurement(marr_tr_t1910, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1910, 0.12).

% Extraction over time
narrative_ontology:measurement(marr_be_t1880, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1880, 0.18).
narrative_ontology:measurement(marr_be_t1887, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1887, 0.22).
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.28).
narrative_ontology:measurement(marr_be_t1900, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(marr_be_t1910, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1910, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1880, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1880, 0.1).
narrative_ontology:measurement(marr_su_t1887, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1887, 0.14).
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.15).
narrative_ontology:measurement(marr_su_t1900, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1900, 0.16).
narrative_ontology:measurement(marr_su_t1910, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1910, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.12).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel marriage_commitment_legitimacy. The kernel is the Manifesto reversal (1890) on plural marriage doctrine. Three structurally distinct readings decompose from this kernel: (1) endogenous_reinterpretation_reading (this file): The Manifesto is genuine divine revelation clarifying God's will; prophetic authority vindicates living revelation doctrine; low extractiveness. (2) exogenous_override_reading: Federal pressure forced capitulation; theology rationalized coercion; high extractiveness. (3) hybrid_pragmatic_reading: Institutional strategy deploying prophetic authority to manage crisis while preserving theological scope ambiguity; moderate extractiveness. Each reading has different epsilon (ε) values, different beneficiary/victim structures, different types. The readings are not measurements of the same constraint via different observables—they are distinct constraints grounded in the same historical kernel. They are linked via network.affects_constraints to enable committer-axis analysis and constraint-family comparison.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
