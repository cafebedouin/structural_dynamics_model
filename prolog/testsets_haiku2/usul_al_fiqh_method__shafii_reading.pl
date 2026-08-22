% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__shafii_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__shafii_reading
 *   human_readable: Shafi'i Usul al-Fiqh Method: Hadith Authentication Prerequisite and Source Hierarchy
 *   domain: legal_theory/religious_jurisprudence
 *
 * SUMMARY:
 *   The Shafi'i reading of usul al-fiqh method systematizes Islamic
 *   jurisprudence around textual authentication as a prerequisite for legal
 *   derivation. The constraint establishes a hierarchical source framework:
 *   Quranic text > authenticated hadith (sahih/hasan) > Companions' consensus
 *   (ijma' al-sahaba) > limited analogical reasoning (qiyas only in the
 *   absence of authenticated hadith). This reading operationalizes
 *   gatekeeping authority in the hands of hadith transmission scholars and
 *   textual authentication specialists, while demoting rationalist juristic
 *   methods and regional practice-based authorities. The constraint is
 *   presented as a solution to fragmentation and unreliable legal reasoning;
 *   measured structurally, it extracts significant authority from competing
 *   methodological traditions and enforces that subordination through the
 *   requirement that all legal claims trace back to authenticated textual
 *   sources. The claim/metric gap is deliberate: the Shafi'i school itself
 *   claims this is coordination (systematic methodology reducing chaos); the
 *   authored metrics describe substantially extractive institutional
 *   gatekeeping with active enforcement mechanisms suppressing alternative
 *   methodologies. The engine measures that divergence per-seat.
 *
 * KEY AGENTS:
 *   - hadith_transmission_scholars: Institutional gatekeepers; their authentication work becomes prerequisite to all legal derivation; benefit from the constraint's elevation of textual authentication
 *   - rationalist_jurists: Organized but identity-locked practitioners of qiyas and ra'y; severely constrained by the subordination of reasoned analogy to textual sources; bear extraction in the form of restricted methodological authority
 *   - regional_practice_authorities: Moderate power, mobile exit; lose independent evidentiary weight for established community practices unless traced to textual sources; regionally dispersed, so suppression cost is lower than for centralized institutions
 *   - companions_consensus_evaluators: Institutional specialists; trap-positioned because their unique role (determining Companions' positions) is created by the constraint itself and becomes essential to the system
 *   - public_interest_advocates: Excluded from discourse; their maslaha reasoning is structurally delegitimized under this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, 0.68).
domain_priors:suppression_score(usul_al_fiqh_method__shafii_reading, 0.72).
domain_priors:theater_ratio(usul_al_fiqh_method__shafii_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__shafii_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__shafii_reading, "Shafi'i Usul al-Fiqh Method: Hadith Authentication Prerequisite and Source Hierarchy").
narrative_ontology:topic_domain(usul_al_fiqh_method__shafii_reading, "legal_theory/religious_jurisprudence").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__shafii_reading, '1f666d78-e2d3-4904-8f36-7759e05fc8dc').
narrative_ontology:cs_kernel_codification('1f666d78-e2d3-4904-8f36-7759e05fc8dc', fixed_text).
narrative_ontology:cs_authority_grounding('1f666d78-e2d3-4904-8f36-7759e05fc8dc', lineage).
narrative_ontology:cs_interpretation_layer_present('1f666d78-e2d3-4904-8f36-7759e05fc8dc').
narrative_ontology:cs_reading_relation('1f666d78-e2d3-4904-8f36-7759e05fc8dc', usul_al_fiqh_method__hanafi_reading, forecloses).
narrative_ontology:cs_reading_relation('1f666d78-e2d3-4904-8f36-7759e05fc8dc', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f666d78-e2d3-4904-8f36-7759e05fc8dc', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('1f666d78-e2d3-4904-8f36-7759e05fc8dc', foundational, textual_authentication_prerequisite).
narrative_ontology:cs_axiom_status(textual_authentication_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('1f666d78-e2d3-4904-8f36-7759e05fc8dc', textual_authentication_prerequisite, empirically_contingent).
narrative_ontology:cs_axiom('1f666d78-e2d3-4904-8f36-7759e05fc8dc', foundational, companions_consensus_unique_authority).
narrative_ontology:cs_axiom_status(companions_consensus_unique_authority, holdable).
narrative_ontology:cs_axiom_grounding('1f666d78-e2d3-4904-8f36-7759e05fc8dc', companions_consensus_unique_authority, deontological).
narrative_ontology:cs_axiom('1f666d78-e2d3-4904-8f36-7759e05fc8dc', secondary, qiyas_subordinate_to_textual_sources).
narrative_ontology:cs_axiom_status(qiyas_subordinate_to_textual_sources, holdable).
narrative_ontology:cs_axiom_grounding('1f666d78-e2d3-4904-8f36-7759e05fc8dc', qiyas_subordinate_to_textual_sources, instrumental).
narrative_ontology:cs_reference_frame('1f666d78-e2d3-4904-8f36-7759e05fc8dc', shafii_textual_hierarchy_framework).
narrative_ontology:cs_drift_state('1f666d78-e2d3-4904-8f36-7759e05fc8dc', contemporary_islamic_jurisprudence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1f666d78-e2d3-4904-8f36-7759e05fc8dc', '2026-08-15T14:32:00Z').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, hadith_transmission_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, textual_authentication_specialists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, regional_practice_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, rationalist_jurists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, companions_consensus_evaluators).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, successor_generation_jurists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and authenticate the corpus of hadith through systematic evaluation of chains of transmission (isnad) and textual consistency. Under this reading, they possess gatekeeping authority over what enters the source hierarchy as admissible evidence for legal derivation. Their credentials and methodologies determine which reports qualify as authenticated (sahih or hasan) and therefore become prerequisites for legal reasoning. Their institutional status depends on the continued subordination of other methodological approaches to hadith authentication.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, hadith_transmission_scholars, agenda_setter,
    institutional, generational, constrained, universal).

% Practitioners of juristic reasoning (ra'y, qiyas, istihsan) who historically claimed authority to derive law through reasoned analogy and juristic preference, especially when textual sources were silent or inconclusive. Under the Shafi'i reading, their scope is severely restricted: qiyas is demoted to a last resort (only when authenticated hadith is absent), and istihsan is effectively prohibited as arbitrary preference. They retain a residual role in filling genuine gaps, but the gatekeeping of what counts as 'authenticated hadith' removes control of their own methodological boundaries. Identity-locked: career structures, scholarly lineages, and institutional positions were built on the authority of reasoned opinion; accepting subordination to hadith authentication requires fundamental reconstitution of professional authority.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, rationalist_jurists, payer,
    organized, biographical, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, rationalist_jurists, beneficiary).

% Scholars of Quranic exegesis (tafsir) and textual interpretation who, under this reading, gain elevated status as their work becomes foundational: Quranic sources carry absolute priority, and all legal derivation anchors to textual meaning. Their interpretive methodologies and philological authorities become the baseline against which secondary sources (hadith, qiyas) are measured. The constraint systematizes them as the primary authoritative layer in usul al-fiqh.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, textual_authentication_specialists, beneficiary,
    institutional, generational, constrained, universal).

% Scholars and judges who historically based legal authority on established practice ('amal) in their communities, particularly in Medina and other regional centers. Under the Shafi'i reading, such practice-based authorities lose independent evidentiary weight unless they can be traced to hadith or Quranic sources. Their regional expertise and customary practice are subordinated to universal textual authentication standards. They retain authority when they can demonstrate textual grounding, but the burden of proof shifts to them.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, regional_practice_authorities, payer,
    moderate, biographical, mobile, regional).

% Scholars who specialize in establishing what positions the Prophet's Companions took on legal matters (ijma' al-sahaba). Under this reading, their work gains unique authority: ijma is restricted to Companions' consensus only, excluding later generations' agreement. This creates a specialized research and adjudication domain: determining what the Companions actually agreed upon becomes a high-stakes gatekeeping function. The constraint traps them because the entire evidentiary apparatus depends on their authentication of historical consensus.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, companions_consensus_evaluators, beneficiary,
    institutional, generational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, companions_consensus_evaluators, agenda_setter).

% Jurists and judges who claim authority to derive law on grounds of public interest (maslaha mursala) when textual sources do not directly address a circumstance. Under the Shafi'i reading, such claims are structurally excluded: public interest reasoning without textual grounding violates the source hierarchy. They would argue that rigid adherence to authenticated texts paralyzes jurisprudence when new circumstances arise, but their voice is systematically positioned outside the legitimate discourse space.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, public_interest_advocates, excluded,
    moderate, biographical, constrained, regional).

% Scholars from outside Islamic jurisprudence who study usul al-fiqh methods and their comparative grounding in different legal-theoretical traditions. They observe which epistemological commitments underlie each reading and how the constraint distribution differs across schools. They have no stake in any particular reading's triumph but provide analysis of the structural differences.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, comparative_legal_authorities, observer,
    analytical, generational, analytical, universal).

% Later-generation jurists (taabi'un and beyond) who would face severe restrictions on their ability to claim ijma or use qiyas to extend law to new circumstances their generation faces. Under the Shafi'i reading, their consensus carries no independent weight (only Companions' consensus counts), and their analogical reasoning is subordinated to what authenticated hadith already covers. Identity-locked: professional legitimacy depends on demonstrating fidelity to the Shafi'i method, which restricts their own creative juristic authority.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, successor_generation_jurists, payer,
    powerless, biographical, identity_locked, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__shafii_reading, hadith_transmission_scholars).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__shafii_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, systematized methodology for legal derivation across Islamic jurisprudence: creates a transparent source hierarchy (Quran → authenticated hadith → Companions' ijma → limited qiyas) that reduces arbitrary juristic disagreement and provides a canonical framework for checking competing legal claims. Solves the coordination problem of how to adjudicate disputes between different jurists' methodologies.
% TRANSFER_FUNCTION: Transfers gatekeeping authority over legal derivation from diverse methodological practitioners (rationalist jurists, regional practice authorities, later-generation consensus-claimers) to specialists in textual authentication (hadith scholars, tafsir scholars, Companions' consensus evaluators). The constraint moves control of what counts as 'legitimate source' into the hands of those who authenticate textual materials.
% ABSENT_VOICES: Rationalist jurists who would defend the authority of reasoned opinion (ra'y) and juristic preference (istihsan); regional practice authorities who would argue for the legal weight of established community customs; public interest advocates who would assert the legitimacy of maslaha mursala reasoning; later-generation scholars who would claim that their collective consensus carries weight equivalent to that of the Companions. These voices are structurally excluded from the discourse space established by the Shafi'i reading.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared, Islamic jurisprudence would reorganize around competing epistemologies: rationalist methodologies would regain scope (qiyas and istihsan applied without hadith prerequisites); regional practices would reassert independent authority; public interest reasoning would operate without textual restriction; later-generation consensus would claim equal weight to Companions' agreement. The systematic hierarchy that Shafi'i usul instituted would collapse, and juristic authority would disperse across multiple competing methodological camps.
% FOUNDING_PROBLEM: Early Islamic jurisprudence faced severe fragmentation: different jurists claimed authority through different methods (some emphasizing analogy, others text, others practice), leading to conflicting rulings on the same questions. Unreliable or forged hadith reports were circulating and being used to justify arbitrary legal positions. There was no systematic framework for evaluating which sources took precedence when they conflicted, and no transparent methodology for determining which rulings were legitimate.
% FOUNDING_PROBLEM_CORROBORATION: Hadith scholars and textual authentication specialists attest the founding problem remains live: unreliable hadith continues to circulate, and systematic methodology remains essential to prevent fabrication and arbitrary reasoning. Rationalist jurists and practitioners of regional jurisprudence attest that the founding problem is substantially solved (modern Islamic societies have canonical collections of authenticated hadith, multiple schools coexist without chaos), and the continued restriction of their methodological scope serves institutional power rather than genuine coordination. Historical-critical scholarship on usul al-fiqh development (from outside the directly benefiting parties) documents that the constraint emerged gradually as part of Shafi'i's own systematization work and represents one possible organizational response, not a necessary solution.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__shafii_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__shafii_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__shafii_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__shafii_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is high because the constraint fundamentally reorders who holds authority to speak legitimately about law: gatekeeping power concentrates in textual authentication specialists. The transfer is asymmetric — rationalist jurists lose scope without equivalent compensation; regional authorities lose independence. Suppression (0.72) is substantial because the constraint must actively maintain the hierarchy: alternative methodologies must be continuously subordinated, and claims to authority through non-textual means must be delegitimized. Theater (0.41, moderate) reflects that the constraint does genuine coordinating work (the source hierarchy reduces some forms of arbitrary disagreement), but an increasing share of enforcement energy (as time progresses, theater rises from 0.22 to 0.41) goes into defending methodological boundaries rather than performing the authentication function itself. The accessibility_collapse (0.76) is high because once the Shafi'i framework is internalized, alternatives appear illegitimate or impossible — a jurist working within the tradition cannot easily claim authority through rationalist methods without violating the framework. Resistance (0.58) is moderate because rationalist jurists and practitioners continue to argue for broader qiyas and ra'y, and regional authorities resist universalized authentication standards, even though they operate within the broader Shafi'i institutional context.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of hadith transmission scholars and textual authentication specialists: this is genuine coordination, a necessary systematization that eliminated chaos and created reliable methodology. From the seat of rationalist jurists: this is institutional gatekeeping that extracts their methodological authority without compensation and locks them into a subordinate role. From the seat of regional practice authorities: this is erasure of legitimate practice-based knowledge in favor of centralized textual standards. The engine computes these per-seat classifications from the structural data (power, exit_options, role). The Shafi'i school's own framing emphasizes coordination and reliability; the metrics emphasize extraction and gatekeeping. Both are authored independently; the divergence is what the corpus measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith transmission scholars face low directionality (d ~ 0.2–0.3): they are beneficiaries whose authority and institutional position depend on the constraint. Their exit options are constrained (their careers are built on this expertise), but their power is institutional and they collect the gatekeeping rents directly. Rationalist jurists face high directionality (d ~ 0.7–0.8): they are targets whose methodological scope is restricted; their exit options are identity-locked (professional identity fused with rationalist jurisprudence); their power is organized but subordinated. Regional practice authorities face moderate directionality (d ~ 0.5–0.6): they lose independent authority but retain some role when their practice can be textualized; their exit options are mobile (they can shift practice to align with textual sources, though this requires institutional change). Successors jurists face very high directionality (d ~ 0.85): they are powerless relative to the institutional gatekeepers, identity-locked in their role as jurists within the Shafi'i tradition, and systematically blocked from claiming independent authority through consensus or analogy. The constraint scales effective extraction upward for these targets because their power is low and their exit is locked.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids the simple mandatrophy trap by retaining a plausible coordination function: the source hierarchy does reduce certain forms of arbitrary disagreement and provides a transparent methodology. However, the founding_problem_status is contested, and the measurement series shows theater_ratio rising from 0.22 to 0.41 over the interval, suggesting that an increasing share of enforcement activity is devoted to defending methodological boundaries rather than performing authentication. This is consistent with a constraint that retains partial legitimate function but is increasingly performing theatrical maintenance of gatekeeping boundaries. The mismatch between founding_problem_status='contested' and disappearance_verdict='world_rearranges' suggests that the constraint's persistence depends on ongoing suppression of alternative methodologies, not on universally acknowledged necessity. The rationalist jurists' identity-lock is the key mechanism: they cannot exit the tradition without abandoning their professional identity, even though the constraint extracts their methodological authority. This prevents coalition formation among payers (rationalist and regional authorities) that would otherwise threaten the constraint. The textual authentication specialists' institutional control prevents the emergence of rival authentication standards — the constraint systematizes the Shafi'i reading as the canonical framework, making it hard for alternative readings to establish competing authorities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_resolution_empirical,
    'Has the founding problem of fragmented jurisprudence and unreliable hadith actually been resolved by Shafi''i systematization, or does the constraint persist despite the problem being substantially solved?',
    'Historical-critical analysis of the distribution and frequency of juristic disagreement pre- and post-Shafi''i systematization; assessment of whether modern hadith authentication standards actually eliminate the reliability problems that motivated the original constraint.',
    'If the problem is substantially solved, the constraint shows mandatrophy: persistence by inertia despite the original need being met. If the problem remains live, the constraint retains legitimate function. The classification could shift from tangled_rope toward piton if solution-evidence is strong.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_resolution_empirical, empirical, 'Whether the constraint''s founding problem persists or has been resolved.').

omega_variable(
    textual_authentication_methodology_contested,
    'Is the methodology of hadith authentication (isnad evaluation, textual consistency checking, narrator reliability assessment) itself scientifically sound and universally accepted, or is it a constructed tradition that benefits authentication specialists without independent epistemic grounding?',
    'Comparative analysis of hadith authentication methodologies against modern documentary evidence standards; assessment of whether the same criteria applied to non-Islamic textual traditions would yield the same results; examination of whether alternative authentication methodologies (applied to the same corpus) would produce substantially different evaluations.',
    'If the methodology is scientifically grounded, the gatekeeping function has legitimate epistemic basis. If it is constructed and tradition-dependent, the constraint''s extraction function becomes more salient: gatekeeping authority is maintained through institutional rather than epistemological necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_authentication_methodology_contested, conceptual, 'Whether hadith authentication methodology is scientifically grounded or institutionally constructed.').

omega_variable(
    qiyas_suppression_mechanism_structural_or_internalized,
    'Is the subordination of qiyas (analogical reasoning) to textual sources structurally enforced (external barriers to legitimacy claims) or internalized (rationalist jurists have fused their professional identity with the Shafi''i framework and cannot imagine exercising broader qiyas authority)?',
    'Post-constraint exit analysis: if jurists who leave the Shafi''i tradition (or historical periods when it was not dominant) immediately assert broader qiyas authority, the suppression is structural. If they continue to internalize the constraint even when external enforcement is removed, the suppression is partially internalized.',
    'If structural, the constraint depends on continued institutional enforcement to persist. If internalized, the constraint carries forward even when institutional gatekeepers lose power, making it more stable but also more difficult to reform. This affects both the extracted authority measure and the theater_ratio: high internalization pushes theater_ratio downward (less enforcement visible, constraint is self-maintaining).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qiyas_suppression_mechanism_structural_or_internalized, empirical, 'Whether qiyas suppression is structural or internalized mechanism.').

omega_variable(
    kernel_reading_foreclosure_relationship,
    'Does the Shafi''i reading logically foreclose the Hanafi expansion of qiyas within a single coherent framework, or can both be held simultaneously with different applications and scopes?',
    'Logical analysis of the core premises: if Shafi''i''s ''qiyas only when authenticated hadith is absent'' is understood as a default epistemic priority rather than an absolute prohibition, could it coexist with Hanafi''s expanded qiyas applied to different domains? Or are they genuinely incompatible?',
    'If they foreclose each other, the kernel instantiates a rare logical contradiction at the framework level. If they coexist, the kernel is a case of genuine pluralism held by different schools. This affects the reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_relationship, conceptual, 'Logical structure of the Shafi''i vs. Hanafi reading relationship.').

omega_variable(
    companions_consensus_restriction_empirical_or_normative,
    'Is the restriction of ijma to Companions'' consensus justified by empirical claims about reliability (Companions were closer to the Prophet, had direct knowledge) or by normative claims (only Companions have the epistemic standing to create binding consensus)?',
    'Analysis of the foundational texts and arguments Shafi''i and later scholars use to justify the restriction. Assessment of whether the empirical claims about Companions'' knowledge are testable or are theological premises.',
    'If primarily empirical, the restriction could be revisited if historical evidence challenges the reliability claim. If primarily normative, the restriction is more stable but also reveals the theological commitments underlying the constraint rather than purely epistemological necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(companions_consensus_restriction_empirical_or_normative, conceptual, 'Whether Companions'' ijma restriction is empirically or normatively grounded.').

omega_variable(
    identity_lock_depth_rationalist_jurists,
    'How deeply are rationalist jurists'' professional identities fused with their practice of qiyas and ra''y within the Shafi''i tradition? Can they retain professional legitimacy while advocating for broader analogical reasoning, or does such advocacy require exit from the tradition entirely?',
    'Historical documentation of rationalist jurists'' positions within Shafii institutional structures; assessment of whether scholars who advocated for expanded qiyas were marginalized or expelled, or whether they retained legitimate standing. Analysis of contemporary juristic discourse.',
    'High identity-lock (exit requires losing professional standing) amplifies the extraction measure and makes coalition formation less likely. Low identity-lock (advocacy possible within the tradition) reduces the extraction measure and increases the possibility of internal reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_depth_rationalist_jurists, empirical, 'Depth of identity-lock for rationalist jurists within Shafi''i tradition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__shafii_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__shafii_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(usul_tr_t8, usul_al_fiqh_method__shafii_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(usul_tr_t16, usul_al_fiqh_method__shafii_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(usul_tr_t24, usul_al_fiqh_method__shafii_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(usul_tr_t32, usul_al_fiqh_method__shafii_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(usul_tr_t40, usul_al_fiqh_method__shafii_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__shafii_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(usul_be_t8, usul_al_fiqh_method__shafii_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(usul_be_t16, usul_al_fiqh_method__shafii_reading, base_extractiveness, 16, 0.64).
narrative_ontology:measurement(usul_be_t24, usul_al_fiqh_method__shafii_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(usul_be_t32, usul_al_fiqh_method__shafii_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(usul_be_t40, usul_al_fiqh_method__shafii_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__shafii_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(usul_su_t8, usul_al_fiqh_method__shafii_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(usul_su_t16, usul_al_fiqh_method__shafii_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(usul_su_t24, usul_al_fiqh_method__shafii_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(usul_su_t32, usul_al_fiqh_method__shafii_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(usul_su_t40, usul_al_fiqh_method__shafii_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__shafii_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__shafii_reading, 0.14).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanbali_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, islamic_jurisprudence_legitimacy_framework).

% DUAL FORMULATION NOTE:
% The kernel 'usul_al_fiqh_method' decomposes into four constraint stories, one per madhhab reading. Each reading instantiates a different source hierarchy with different beneficiaries and victims. The Shafi'i reading centralizes authority in textual authentication specialists; the Hanafi reading distributes it more broadly across rationalist methodologies; the Maliki reading incorporates regional practice; the Hanbali reading maximizes textual restriction. These are not measurement variations on a single constraint — they are structurally distinct constraints with different ε values, different victim sets, and different classification outcomes. They are linked via network edges to enable cross-reading contamination and competition analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__shafii_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
