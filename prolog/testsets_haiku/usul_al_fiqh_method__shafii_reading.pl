% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Shafii Usul al-Fiqh Method: Hadith Authentication as Source Hierarchy Gate
 *   domain: religious/jurisprudential/intellectual
 *
 * SUMMARY:
 *   The Shafii school of Islamic jurisprudence institutionalizes a specific
 *   methodological hierarchy of legal sources: authenticated hadith stands as
 *   the prerequisite gateway to all legal derivation; analogical reasoning
 *   (qiyas) is permissible only in the absence of authenticated textual
 *   sources; consensus (ijma) is recognized only when traceable to the
 *   Companions of the Prophet. This constraint formalizes the Shafii reading
 *   of usul al-fiqh (principles of jurisprudence), one of four major readings
 *   that emerged in early Islamic legal history. The other readings (Hanafi,
 *   Maliki, Hanbali) recognize different hierarchies, where rationalist
 *   analogical reasoning (Hanafi), customary practice (Maliki), or textual
 *   minimalism (Hanbali) occupy different ranks. The Shafii method elevates
 *   hadith scholars and transmitters as the gatekeepers of legal authority—no
 *   jurist can bypass their authentication verdicts. Simultaneously, it
 *   subordinates jurists who practice extensive analogical reasoning without
 *   textual bases, reducing their methods to a residual tier. The constraint
 *   is simultaneously coordinative (it provides explicit source hierarchy,
 *   reducing arbitrary legal divergence) and extractive (it concentrates
 *   authority in one professional group and suppresses alternative
 *   methodologies). The Shafii institutional authorities enforce this
 *   constraint through teaching, transmission of the school's doctrine, and
 *   the organization of courts and fatwa councils that recognize only
 *   Shafii-authenticated sources. The measurement series shows extractiveness
 *   rising from 0.45 (early systematization period) to 0.68 (institutional
 *   consolidation), then stabilizing—the founding institutional burden of
 *   establishing the hierarchy requires high active enforcement; once the
 *   constraint is normalized across generations, maintenance costs decline
 *   relative to the extraction benefit retained.
 *
 * KEY AGENTS:
 *   - hadith_scholars_and_transmitters: Specialists in authenticating hadith reports via isnad (transmission chain) verification; primary beneficiaries of the constraint's gatekeeping structure
 *   - shafii_school_institutional_authority: Organized schools, teaching centers, courts, and fatwa councils administering and enforcing the Shafii methodological frame
 *   - rationalist_jurists: Jurists (especially Hanafi-influenced) claiming authority through unaided qiyas and ra'y; primary victims of subordination
 *   - non_shafii_schools: Hanafi, Maliki, Hanbali institutional structures operating under alternative source hierarchies; excluded from Shafii jurisdictions but not eliminated
 *   - competing_usul_schools: Analytical observers of the methodological plurality; measure whether Shafii systematization is inevitable or contingent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, 0.68).
domain_priors:suppression_score(usul_al_fiqh_method__shafii_reading, 0.71).
domain_priors:theater_ratio(usul_al_fiqh_method__shafii_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__shafii_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__shafii_reading, "Shafii Usul al-Fiqh Method: Hadith Authentication as Source Hierarchy Gate").
narrative_ontology:topic_domain(usul_al_fiqh_method__shafii_reading, "religious/jurisprudential/intellectual").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__shafii_reading, '39d59098-b535-4632-a446-c8a4094d916d').
narrative_ontology:cs_kernel_codification('39d59098-b535-4632-a446-c8a4094d916d', formalized).
narrative_ontology:cs_authority_grounding('39d59098-b535-4632-a446-c8a4094d916d', lineage).
narrative_ontology:cs_interpretation_layer_present('39d59098-b535-4632-a446-c8a4094d916d').
narrative_ontology:cs_reading_relation('39d59098-b535-4632-a446-c8a4094d916d', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('39d59098-b535-4632-a446-c8a4094d916d', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('39d59098-b535-4632-a446-c8a4094d916d', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('39d59098-b535-4632-a446-c8a4094d916d', foundational, hadith_authentication_prerequisite_to_legal_derivation).
narrative_ontology:cs_axiom_status(hadith_authentication_prerequisite_to_legal_derivation, holdable).
narrative_ontology:cs_axiom_grounding('39d59098-b535-4632-a446-c8a4094d916d', hadith_authentication_prerequisite_to_legal_derivation, deontological).
narrative_ontology:cs_axiom('39d59098-b535-4632-a446-c8a4094d916d', foundational, qiyas_authority_subordinate_to_textual_sources).
narrative_ontology:cs_axiom_status(qiyas_authority_subordinate_to_textual_sources, holdable).
narrative_ontology:cs_axiom_grounding('39d59098-b535-4632-a446-c8a4094d916d', qiyas_authority_subordinate_to_textual_sources, conventional).
narrative_ontology:cs_reference_frame('39d59098-b535-4632-a446-c8a4094d916d', textually_authenticated_source_hierarchy).
narrative_ontology:cs_drift_state('39d59098-b535-4632-a446-c8a4094d916d', contemporary_comparative_legal_analysis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('39d59098-b535-4632-a446-c8a4094d916d', '2026-06-11T00:00:00Z').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, hadith_scholars_and_transmitters).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, shafii_school_institutional_authority).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, jurists_employing_extensive_qiyas).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, non_companion_ijma_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, learned_hadith_community_collective).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, jurists_claiming_non_companion_ijma).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the authentication criteria (isnad validation, reliability assessment of transmitters) by which hadith enter the legal canon. Their professional authority depends on demonstrating mastery of transmission chains and recognizing weak reports. The Shafii method elevates their gatekeeping function: no legal rule can derive from sources they reject. They benefit directly from the constraint's enforcement—it makes their authentication work prerequisite to all jurisprudential argument, not optional reference.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, hadith_scholars_and_transmitters, agenda_setter,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, hadith_scholars_and_transmitters, beneficiary).

% Maintains and enforces the Shafii methodological framework across generations of schools, courts, and teaching institutions. Administers which hadith are accepted, which analogical inferences are valid, which consensus claims are recognized. The constraint is their institutional law: it defines what counts as legitimate jurisprudential reasoning within the school.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, shafii_school_institutional_authority, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Jurists (particularly Hanafi scholars and independent reasoners) who claim authority to derive law through unaided rational analogy (qiyas) and juristic preference (istihsan) when texts are silent. Under the Shafii method they are permanently subordinated: their analogies are only permissible after exhausting authenticated hadith, and their reasoned preferences have no independent standing. They bear professional subordination—their methods are not invalidated outright, but are reduced to a residual, lower-ranked tier of inference. Exit is identity-locked: leaving jurisprudence itself or renouncing the school are the only exits; staying within Shafii jurisprudence means accepting the hierarchy.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, rationalist_jurists, payer,
    moderate, biographical, identity_locked, continental).

% Jurists who attempt to ground legal rules in the consensus of later scholars (post-Companion generations). The Shafii method forecloses this: ijma is recognized only when it can be traced to the Companions themselves (the most authenticated generation). Later jurists claiming consensus find their consensus-based arguments rejected as lacking the required evidentiary weight. They experience professional cost: their consensus-building work is deemed methodologically inferior unless retrofitted to Companion-era sources.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, jurists_claiming_non_companion_ijma, payer,
    moderate, biographical, constrained, continental).

% Hanafi, Maliki, and Hanbali schools that recognize different hierarchies of sources (expanded qiyas, public interest, custom, broader ijma). Within Shafii institutional jurisdiction, their methods are present but excluded from authority-setting. They have parallel institutional structures and large constituencies, but where Shafii law governs (courts, schools, fatwa authorities), their methodologies cannot set binding precedent. Their exclusion is structural, not political: the constraint defines what counts as valid reasoning, so schools using different reasoning patterns are structurally out of bounds in Shafii contexts.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, non_shafii_schools, excluded,
    powerful, civilizational, trapped, continental).

% The broader network of hadith collectors, critics, and preservers who profit from the Shafii method's elevation of hadith authentication as the primary gateway to jurisprudential authority. Their work becomes indispensable—no jurist can bypass their authentication verdicts. They gain cultural prestige, institutional position, and influence over legal outcomes via their gatekeeping role, even when they hold no formal judicial office.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, learned_hadith_community_collective, beneficiary,
    organized, generational, mobile, continental).

% Scholars of other methodological frameworks (Hanafi usul scholars, Maliki usul theorists) who analyze, critique, or systematize alternative hierarchies of legal sources. They observe Shafii usul as one systematization among several, each with internal logic and institutional backing. Their analytical work measures whether Shafii systematization is structurally inevitable or one viable choice among others.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, competing_usul_schools, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__shafii_reading, hadith_scholars_and_transmitters).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__shafii_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, explicit hierarchy of legal sources (authenticated hadith > qiyas > restricted ijma) so that jurists across generations can derive law using consistent methods. Systematizes jurisprudential reasoning to reduce arbitrary disagreement and produce coherent school-level doctrine. Enables transmission and authentication of hadith as a continuous disciplinary practice by making it essential to all legal reasoning.
% TRANSFER_FUNCTION: Transfers gatekeeping authority from individual rationalist jurists to organized hadith scholars—their authentication verdicts become prerequisite to all legal derivation. Jurists who previously could justify conclusions via reasoned analogy alone now must defer to hadith specialists' textual verdicts. Restricts the range of permissible ijma claims, concentrating consensus authority in a narrower (Companion-era) set.
% ABSENT_VOICES: Jurists from other schools (Hanafi, Maliki, Hanbali) who practice within Shafii jurisdictions are structurally present but voice-excluded from setting the methodological frame. Rationalist jurists and analogical reasoners would argue for broader qiyas authority and juristic preference (istihsan) but are subordinated by the constraint's hierarchy. Pre-Companion generations would defend post-Companion ijma as legitimate, but are excluded by the constraint's restriction.
% DISAPPEARANCE_RATIONALE: If the Shafii method's source hierarchy vanished, the entire institutional basis for Shafii jurisprudence would collapse. Jurists would revert to competing methodologies; hadith scholars would lose their gatekeeping role; legal reasoning would fragment across the school into multiple sub-schools following different source hierarchies (as occurred historically when single-school dominance weakened). Courts and teaching institutions would reorganize around different usul principles.
% FOUNDING_PROBLEM: Early Islamic jurisprudence (8th-9th centuries) faced methodological chaos: jurists disagreed on how to derive law from sources, with some relying excessively on personal reasoning (ra'y) and analogical extension (qiyas) beyond the text, others preferring isolated hadith reports of uncertain authenticity, and consensus claims proliferating without clear authentication. Legal rulings diverged wildly even within the same geographic and temporal context.
% FOUNDING_PROBLEM_CORROBORATION: Hadith scholars and Shafii institutional authorities attest that methodological systematization solved the chaos—it provided clear criteria for textual authentication and ranking, reducing arbitrary disagreement. Rationalist jurists and scholars of non-Shafii schools attest that the founding problem was over-diagnosed—divergence in jurisprudential conclusions reflected legitimate methodological pluralism, not chaos requiring suppression of qiyas and reasoned opinion. Historians of Islamic law outside the Shafii school document that methodological pluralism persisted and produced stable, coherent sub-traditions (Hanafi, Maliki, Hanbali schools all systematized their own usul), suggesting Shafii systematization solved a school-internal problem, not an Islamic-legal problem writ large.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__shafii_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__shafii_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.68 at stabilization) because the constraint concentrates legal authority in hadith scholars' hands—jurists cannot override their authentication verdicts through reasoned analysis alone. Suppression is higher (0.71) because maintaining the hierarchy requires actively excluding or subordinating competing methodologies; this is not passive natural law but sustained institutional gatekeeping. Theater is relatively low (0.28) because the constraint's functional purpose (providing source hierarchy) is real and ongoing—it is not merely performative. Accessibility_collapse is high (0.82) because once the Shafii framework is established, alternatives (rationalist jurisprudence, post-Companion ijma, other usul schools) are closed off within Shafii-governed jurisdictions; jurists cannot simply choose a different methodology if they work within the school. Resistance is moderate (0.59) because rationalist and alternative-methodology jurists do mount continuous resistance (developing counter-arguments, maintaining parallel schools, invoking istihsan and maslaha where Shafii constraints allow), but this resistance is subordinated, not successful in overturning the hierarchy. The measurement series tracks institutional consolidation: extractiveness rises as the constraint is internalized into curricula and court systems (T=0-20), then plateaus once normalization is complete (T=25-40). This plateau with non-zero theater suggests the constraint is maintained not by continuous coercion but by institutional inertia and intellectual tradition—the rise in theater_ratio from 0.12 to 0.28 reflects growing emphasis on methodological purity and pedagogical transmission as enforcement mechanism, replacing earlier direct suppression. Suppression_requirement also rises and plateaus, indicating that early enforcement burden (convincing jurists to abandon qiyas authority) has given way to maintenance burden (ensuring schools transmit the hierarchy correctly).
 *
 * PERSPECTIVAL GAP:
 *   The Shafii institutional authority and hadith scholar seats compute this as pure rope: a coordinative solution to methodological chaos that naturally advantages those mastering textual authentication. From their position, the constraint is a discovered necessity—rationalist jurisprudence without textual grounding is incoherent, so textual gates are inevitable. Rationalist jurists and proponents of extensive qiyas compute this as snare: a system of gatekeeping that suppresses methodological pluralism and concentrates authority to benefit one professional class. From their position, the constraint is a deliberate institutional choice that could be relaxed or restructured without collapsing jurisprudential coherence. The engine computes per-seat classification from the structural data (beneficiary/victim declarations, exit_options, power atoms); this divergence—same constraint, different computed types—is the core signal the corpus exists to measure. The claim (tangled_rope) reflects the author's assessment that BOTH readings have structural merit: real coordination + real extraction. Alternative seat orderings would shift the balance.
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith scholars and the Shafii institutional authority are the primary beneficiaries—their directionality d values are near the beneficiary end (0.0-0.2). They collect concrete benefit (authority, status, institutional position, transmission of school). Their exit options (arbitrage for the institutional authority, identity_locked for individual scholars) determine whether they can be displaced; their high organizational power makes them hard to dislodge. Rationalist jurists and qiyas proponents are the primary targets—their directionality d values are near the target end (0.8-1.0). They bear subordination (their methods are ranked lower, their conclusions subject to hadith scholars' authentication verdicts) with high suppression and constrained exit (identity_locked: leaving jurisprudence or the school are the only escapes). Non-Shafii schools are excluded rather than extracted from—their directionality is not computed into the Shafii constraint because they operate in parallel institutional structures, not within Shafii jurisdiction. The constraint's asymmetry is captured in the beneficiary/victim declarations and reinforced by exit_options: hadith scholars can arbitrage (move between schools if needed) or remain identity-locked in the school with low cost; rationalist jurists are identity-locked with high cost (their professional identity is built on reasoning authority, which the constraint denies them).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy—when a constraint's founding purpose has died but the constraint persists—is contested here. The founding problem was 'methodological chaos: jurists disagreed wildly on source hierarchy.' Has this problem been solved or has it merely been suppressed? Shafii authorities attest the problem is solved: the systematized source hierarchy produces coherent jurisprudence within the school. Rationalist jurists and historians attest the problem was over-diagnosed: alternative schools (Hanafi, Maliki, Hanbali) also systematized their own usul and produced equally coherent, stable jurisprudence without subordinating qiyas as thoroughly. If the latter reading is correct, the founding problem (lack of systematization) was solvable without the Shafii hierarchy, making the constraint's persistence an artifact of institutional power rather than functional necessity—this is mandatrophy. The omega 'methodological_pluralism_suppression' directly addresses this: if systematization could coexist with broader qiyas recognition, the Shafii choice is not necessity but extraction. The measurement plateau (extractiveness flat from T=25-40) with rising theater suggests post-mandatrophy dynamics: the constraint is maintained through pedagogical transmission and institutional inertia, not because the founding problem requires it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_distinction,
    'Is the Shafii usul method a discovered reflection of the logical requirements of Islamic law, or is it a methodological choice among structurally equivalent alternatives?',
    'Comparative analysis of non-Shafii schools: if Hanafi, Maliki, and Hanbali usul produce equally coherent, internally consistent jurisprudential systems without resolving into the Shafii hierarchy, the Shafii method is a choice, not a necessity. If alternative schools ultimately converge toward Shafii principles under pressure to reduce internal contradiction, the Shafii method approaches necessity.',
    'If necessary: the constraint is closer to a mountain (inevitable structure), and its beneficiaries are benefiting from coordination that had to occur. If a choice: the constraint is cleanly tangled_rope—real coordination coupled with benefit concentration, neither necessary nor natural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Whether Shafii usul is structurally inevitable or one valid methodological choice among others').

omega_variable(
    gatekeeping_authority_mechanism,
    'How much of the suppression (0.71) comes from explicit institutional enforcement by Shafii authorities versus from internalized acceptance of hadith scholars'' methodological authority by rationalist jurists themselves?',
    'Historical examination of disputes: were rationalist jurists and qiyas-proponents silenced by institutional pressure (courts rejecting their arguments, schools expelling them), or did they accept subordination via intellectual persuasion (coming to believe hadith authentication was methodologically superior)? Post-collapse evidence: where Shafii institutional enforcement weakened, did rationalist jurisprudence re-emerge, suggesting suppression was structural?',
    'If structural suppression dominates: the constraint is maintained by institutional power, and removing enforcement would expose the underlying extraction. If internalized dominates: the constraint''s persistence runs deeper—even absent enforcement, the intellectual frame persists, and this reading''s methodological claims have genuine intellectual hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_authority_mechanism, empirical, 'Whether suppression is structural enforcement or internalized intellectual acceptance').

omega_variable(
    sibling_reading_foreclosure,
    'Do the Shafii and Hanafi readings of usul al-fiqh logically foreclose each other, or do they coexist as live positions within Islamic jurisprudence?',
    'Formal analysis of their core premises: Shafii asserts hadith authentication is prerequisite (qiyas only when hadith absent); Hanafi asserts qiyas is broadly applicable (ra''y and istihsan supplement where analogy reaches limits). Can both premises be true in a single framework? If no single legal system can hold both simultaneously, they foreclose; if different schools/communities can hold them in parallel, they coexist.',
    'If foreclose: one reading''s victory requires the other''s intellectual defeat; classification approaches snare (one reading suppresses a logically coherent alternative). If coexist: both readings are live methodological choices; classification remains tangled_rope (coordination + extraction, neither ruled out by logical necessity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether Shafii and Hanafi usul readings logically foreclose each other or coexist as alternative methodologies').

omega_variable(
    methodological_pluralism_suppression,
    'Is the subordination of rationalist jurisprudence (qiyas, ra''y, istihsan) a necessary consequence of systematizing Islamic legal sources, or a contingent choice to privilege hadith scholars?',
    'Thought experiment: a systematized usul that ranked sources as (1) Quran, (2) hadith, (3) qiyas, (4) ijma (without Companion restriction) would still provide explicit hierarchy and reduce arbitrariness. Does this alternative system solve the ''founding problem'' equally well? If yes, the Shafii method''s subordination of qiyas is not necessitated by systematization, but by a deliberate choice to privilege textual sources over reasoned analogy.',
    'If rationalist methods could be integrated into systematization without subordination: the Shafii choice is revealed as extraction-laden—picking one viable method over others and gatekeeping authority via that choice. If Shafii hierarchy is the only coherent systematic approach: the extraction is more justified as coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(methodological_pluralism_suppression, conceptual, 'Whether hadith prioritization over qiyas is necessary to systematic jurisprudence or a contingent choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__shafii_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__shafii_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(usul_tr_t5, usul_al_fiqh_method__shafii_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(usul_tr_t10, usul_al_fiqh_method__shafii_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(usul_tr_t15, usul_al_fiqh_method__shafii_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(usul_tr_t20, usul_al_fiqh_method__shafii_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(usul_tr_t25, usul_al_fiqh_method__shafii_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(usul_tr_t30, usul_al_fiqh_method__shafii_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(usul_tr_t40, usul_al_fiqh_method__shafii_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__shafii_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(usul_be_t5, usul_al_fiqh_method__shafii_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(usul_be_t10, usul_al_fiqh_method__shafii_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(usul_be_t15, usul_al_fiqh_method__shafii_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(usul_be_t20, usul_al_fiqh_method__shafii_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(usul_be_t25, usul_al_fiqh_method__shafii_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(usul_be_t30, usul_al_fiqh_method__shafii_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(usul_be_t40, usul_al_fiqh_method__shafii_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__shafii_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(usul_su_t5, usul_al_fiqh_method__shafii_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(usul_su_t10, usul_al_fiqh_method__shafii_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(usul_su_t15, usul_al_fiqh_method__shafii_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(usul_su_t20, usul_al_fiqh_method__shafii_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(usul_su_t25, usul_al_fiqh_method__shafii_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(usul_su_t30, usul_al_fiqh_method__shafii_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(usul_su_t40, usul_al_fiqh_method__shafii_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__shafii_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__shafii_reading, 0.18).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% The usul al-fiqh method is a contested kernel with four sibling readings, each instantiating a different constraint. All four share the kernel (systematize Islamic legal sources) but deliver different source hierarchies. Shafii reading prioritizes hadith authentication; Hanafi reading expands qiyas and ra'y; Maliki reading integrates practice and unrestricted public interest; Hanbali reading maximizes textual restriction. The readings coexist as live institutional positions held by different schools, each with independent epistemic justification within its own framework. Shafii reading influences (but does not foreclose) the others by establishing the model of explicit source hierarchy—even schools rejecting Shafii rankings learned systematization from the Shafii example. Omegas in all four readings track whether the readings foreclose or merely coexist.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__shafii_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
