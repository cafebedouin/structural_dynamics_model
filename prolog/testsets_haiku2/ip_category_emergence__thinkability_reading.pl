% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__thinkability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__thinkability_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: ip_category_emergence__thinkability_reading
 *   human_readable: IP Category Emergence: Thinkability Reading (1710)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This reading instantiates ONE specific decomposition of the contested
 *   kernel ip_category_emergence: it emphasizes the process by which
 *   'copyright' became a legally coherent, thinkable category — a conceptual
 *   space that did not exist as a unified frame before ~1710. Pre-1710,
 *   disputes over copying rights were scattered across guild privilege
 *   (Stationers' Company monopoly), Crown patents (individual grants), and
 *   common-law tort (unfair competition, restraint of trade). Post-1710,
 *   'copyright' emerges as a distinct legal concept, enabling coherent
 *   rights-claims, future legislation, and doctrinal development. This
 *   reading isolates THINKABILITY (the conceptual coherence enabling
 *   reasoning about copy-as-property) as the constraint-defining moment. It
 *   is not claiming that first holdings shifted or that subjects changed —
 *   those are separate readings. It is claiming that a category emerged that
 *   made coherent reasoning about copy-ownership possible when it was not
 *   before.
 *
 * KEY AGENTS:
 *   - Legal discourse participants (institutional): benefit from conceptual coherence for dispute resolution
 *   - Authors and printers (moderate power): both benefit from the coherent category and become subject to the enforcement machinery it enables
 *   - Common-law adjudicators (institutional agenda-setters): establish precedent recognizing copy disputes as a distinct category
 *   - Stationers' Guild (organized, now payer): loses monopoly gate-keeping authority as guild privilege recedes
 *   - Pre-1710 disputing parties (phantom/excluded): historically present but analytically excluded from the post-1710 frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, 0.62).
domain_priors:suppression_score(ip_category_emergence__thinkability_reading, 0.41).
domain_priors:theater_ratio(ip_category_emergence__thinkability_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__thinkability_reading, rope).
narrative_ontology:human_readable(ip_category_emergence__thinkability_reading, "IP Category Emergence: Thinkability Reading (1710)").
narrative_ontology:topic_domain(ip_category_emergence__thinkability_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__thinkability_reading, '58ab4f99-ee1f-4229-9ebe-cf20823031fa').
narrative_ontology:cs_kernel_codification('58ab4f99-ee1f-4229-9ebe-cf20823031fa', fixed_text).
narrative_ontology:cs_authority_grounding('58ab4f99-ee1f-4229-9ebe-cf20823031fa', lineage).
narrative_ontology:cs_interpretation_layer_present('58ab4f99-ee1f-4229-9ebe-cf20823031fa').
narrative_ontology:cs_reading_relation('58ab4f99-ee1f-4229-9ebe-cf20823031fa', ip_category_emergence__first_holding_reading, influences).
narrative_ontology:cs_reading_relation('58ab4f99-ee1f-4229-9ebe-cf20823031fa', ip_category_emergence__synchronic_diachronic_seam, coexists_with).
narrative_ontology:cs_axiom('58ab4f99-ee1f-4229-9ebe-cf20823031fa', foundational, coherent_copy_concept_precedes_coherent_claims).
narrative_ontology:cs_axiom_status(coherent_copy_concept_precedes_coherent_claims, holdable).
narrative_ontology:cs_axiom_grounding('58ab4f99-ee1f-4229-9ebe-cf20823031fa', coherent_copy_concept_precedes_coherent_claims, deontological).
narrative_ontology:cs_axiom('58ab4f99-ee1f-4229-9ebe-cf20823031fa', secondary, category_emergence_enables_doctrinal_development).
narrative_ontology:cs_axiom_status(category_emergence_enables_doctrinal_development, holdable).
narrative_ontology:cs_axiom_grounding('58ab4f99-ee1f-4229-9ebe-cf20823031fa', category_emergence_enables_doctrinal_development, instrumental).
narrative_ontology:cs_reference_frame('58ab4f99-ee1f-4229-9ebe-cf20823031fa', coherent_copy_ownership_category).
narrative_ontology:cs_drift_state('58ab4f99-ee1f-4229-9ebe-cf20823031fa', contemporary_ip_jurisprudence, gap(stable, minor, true)).
narrative_ontology:cs_created_at('58ab4f99-ee1f-4229-9ebe-cf20823031fa', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(ip_category_emergence__thinkability_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, legal_discourse_participants).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, authors_and_printers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, authors_and_printers).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, stationers_guild).
narrative_ontology:constraint_vindicates(ip_category_emergence__thinkability_reading, property_extensibility_to_intangible_works).
narrative_ontology:constraint_vindicates(ip_category_emergence__thinkability_reading, copy_ownership_as_coherent_category).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain conceptual coherence for disputes that pre-1710 lacked vocabulary. After 1710, the category 'copyright' becomes stable enough to permit formalized claims, legislation, and litigation strategy. The category's emergence enables legal reasoning that was structurally impossible before the concept existed as a named, bounded phenomenon.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, legal_discourse_participants, beneficiary,
    institutional, generational, analytical, national).

% Pre-1710, disputes over manuscript copying, printing rights, and reissue were adjudicated under guild privilege, unfair competition, and common-law restraint doctrines — conceptually scattered. Post-1710, 'copy right' emerges as a distinct category; authors and printers can now make rights-claims that reference a coherent legal concept. They also become subject to the enforcement machinery the category makes possible (term limits, licensing regimes, piracy prosecution).
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, authors_and_printers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__thinkability_reading, authors_and_printers, payer).

% Judges and legal commentators who recognize copy disputes as a category distinct from guild regulation. They establish precedent that gradually stabilizes 'copyright' as a separate doctrinal field, enabling future lawmakers to legislate copyright distinctly from patent, trademark, and trade secret.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, common_law_adjudicators, agenda_setter,
    institutional, generational, analytical, national).

% Pre-1710, the Stationers' Company held monopoly printing grants and controlled disputes through internal regulation and Crown privilege. Post-1710, as 'copyright' emerges as a distinct legal category, guild privilege recedes and individual author/printer claims compete with guild authority. The guild's grip on the category is eroded, though guild members continue to participate under the new framework.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, stationers_guild, payer,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__thinkability_reading, stationers_guild, excluded).

% The category's emergence at common law enables statutory copyright to follow (Statute of Anne, 1710, or shortly after, depending on reading). Without conceptual coherence, statutory intervention has no stable referent to codify. As the category stabilizes, legislation becomes possible.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, future_legislative_authority, observer,
    institutional, generational, analytical, national).

% Participants in pre-1710 disputes over manuscript copying and reissue rights. Historically, they existed; analytically, they are excluded from the post-1710 discourse because the category they operated under (guild privilege, unfair competition tort) is no longer the live frame. They are preserved in this story only as a phantom class whose disputes provided the raw material the category emerged to resolve.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, pre_1710_disputing_parties, excluded,
    analytical, biographical, analytical, national).
narrative_ontology:stakeholder_non_agent(ip_category_emergence__thinkability_reading, pre_1710_disputing_parties).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__thinkability_reading, legal_discourse_participants).
narrative_ontology:fixing_cost_class(ip_category_emergence__thinkability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, bounded conceptual category — 'copyright' — that enables legal actors to classify disputes that previously required ad-hoc reference to guild privilege, common-law tort, and Crown patent authority. The category is coherent enough to support consistent adjudication and legislative intervention.
% TRANSFER_FUNCTION: Does not move resources directly; rather, moves conceptual authority. Pre-1710 disputes were adjudicated under guild and Crown prerogative frames; post-1710, the category 'copyright' becomes the primary frame, shifting legitimate claimant-status from guild-member status to individual author/printer status. This is a transfer of adjudicative authority, not money.
% ABSENT_VOICES: Continental legal traditions (Roman law, canonical law traditions) that might have organized copy-ownership differently; scribal guilds in non-English jurisdictions operating under different frameworks; printers and authors who benefited from guild monopoly and lost power when 'copyright' disintermediated guild authority.
% DISAPPEARANCE_RATIONALE: If the category 'copyright' had not emerged — if disputes remained adjudicated only under guild privilege and unfair competition tort — legal reasoning would persist but would lack the doctrinal stability to support later statutory regimes. The category's absence does not eliminate disputes; it eliminates the conceptual coherence necessary for systemic legal intervention. Different reading: the category emerged inevitably from the prior conceptual landscape; its disappearance is counterfactual and unresolvable.
% FOUNDING_PROBLEM: Pre-1710, disputes over who held legitimate authority to copy, reissue, or restrict copying were ubiquitous but conceptually scattered across guild regulation, Crown patent authority, and common-law tort doctrine. No unified frame existed for reasoning about 'copy as property' — claims to copy-ownership lacked a coherent legal vocabulary.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (Drahos, Ginsburg, Sherman) document the pre-1710 conceptual incoherence from primary sources (guild records, court proceedings, Crown patents) that show disputes adjudicated under different frames depending on forum and party status. The Stationers' Company records and Star Chamber proceedings establish that copy-rights claims existed but were not yet organized as a distinct legal category. Post-1710 legal commentary and statutory language (Statute of Anne, if dated to 1710) use 'copyright' as a term of art in a way pre-1710 sources do not.
narrative_ontology:disappearance_verdict(ip_category_emergence__thinkability_reading, contested).
narrative_ontology:founding_problem_status(ip_category_emergence__thinkability_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__thinkability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ip_category_emergence__thinkability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__thinkability_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__thinkability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ip_category_emergence__thinkability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.62 at interval end because the emerging category enables new forms of institutional control: once 'copyright' is thinkable and coherent, legislators, judges, and enforcement bodies can impose term limits, licensing regimes, and piracy prosecution — coordination functions that benefit discourse participants but also extract from would-be copiers. Suppression is moderate (0.41) because the category's emergence is fundamentally about opening new conceptual space, not about coercing compliance; the suppression required is primarily conceptual (ruling out alternative frames like pure guild privilege or pure tort law), not physical. Theater is low-moderate (0.28) because the category emerges from genuine legal reasoning about incoherence in pre-existing disputes — it solves a real problem — but some performative work is required to establish it as THE frame rather than ONE competing frame. The measurement series show a rising trajectory across the 50-year interval (1680–1730), capturing the process of emergence: pre-1710 extraction is low (disputes are still adjudicated under legacy frames), and post-1710 extraction rises as the coherent category enables new control mechanisms. The shared time grid ensures every metric is authored at every examined year.
 *
 * PERSPECTIVAL GAP:
 *   Institutional seats (judges, legal commentators, future legislators) experience the category as enabling coherent future governance — they read it as rope. Guild and monopoly-holders read it as disintermediating their authority — they read it as snare. Individual authors and printers read it as enabling their own claims but also subjecting them to new enforcement — they read it as tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Legal discourse participants and authors/printers are beneficiaries: they gain the ability to make coherent claims that were impossible before the category existed. Their directionality is beneficiary-end (low d). The Stationers' Guild is a payer: they lose monopoly authority as individual author/printer claims compete with guild privilege under the new frame. Their directionality is target-end (high d). Common-law adjudicators are agenda-setters (they establish the category); their directionality is ambiguous (they both enable coordination and concentrate adjudicative authority). Future legislators are observers (they inherit the category as a stable referent for statutory intervention).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does not imply mandatrophy; the founding problem (conceptual incoherence in copy disputes) remains live post-1710. The category emerges TO SOLVE the problem, and the solution is broadly durable — later legislation builds on the category successfully. However, the reading remains alert to the possibility that the category might come to serve purposes other than solving the founding problem: if, over time, copyright doctrine evolved to extract monopoly rents unrelated to coherence or dispute resolution, the category would begin to lose connection to its founding problem. This measurement series captures the early period (1680–1730) during which the category is establishing itself; a longer interval would show whether the founding problem's relevance persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    category_emergence_causation,
    'Did the legal category ''copyright'' emerge from conceptual necessity (disputes demanded a coherent frame) or from institutional choice (judges and lawmakers deliberately invented the category)?',
    'Trace primary sources pre- and post-1710 to determine whether (a) legal reasoning shifted to use ''copyright'' deliberately or (b) the term appeared gradually as an emergent descriptive label applied to pre-existing dispute patterns.',
    'If emergent necessity: the constraint is structurally coordinative (solving a real conceptual gap). If institutional choice: the constraint is more extractive (imposed vocabulary concentrating authority). The reading''s own premise is that emergence is THE operative fact, independent of causation; if causation differs, the reading''s structure shifts from coordination to authority-consolidation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_emergence_causation, empirical, 'Whether category emergence was driven by conceptual necessity or institutional deliberation.').

omega_variable(
    thinkability_vs_holding_separability,
    'Is the emergence of ''copyright'' as a thinkable category logically independent from the substantive shift in who holds copy rights (from guild to author)? Or is thinkability the necessary precondition for the holding shift?',
    'M4/M5 collapse test: if removing thinkability-emergence still permits the holding shift (authors make rights-claims under existing frames), then thinkability is not logically prerequisite to holding. If removing thinkability forecloses the holding shift (no stable concept, no rights-claim structure), then thinkability and holding are tightly coupled.',
    'If separable: thinkability and first_holding_reading are two distinct constraints with measurable independence. If coupled: the kernel decomposition into three siblings may be over-cut; ''thinkability'' may be downstream of ''first holding'' rather than upstream. This omega routes the framing-under-determination (whether category emergence or subject-position emergence is the kernel''s core) through the apparatus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(thinkability_vs_holding_separability, conceptual, 'Whether thinkability (conceptual coherence) and first_holding (subject-position shift) are separable or inseparably coupled.').

omega_variable(
    reading_neutral_referent,
    'Is the referent for epsilon measurement the process of category emergence itself (thinkability becoming possible), or the institutional regime that the category enabled (the post-1710 copyright system)? Does ε describe the emergence event or the emerged structure?',
    'Fix ε by choosing: (a) ε = extractiveness of the PROCESS of making ''copyright'' thinkable (institutional effort, conceptual labor, winnowing competing frames), or (b) ε = extractiveness of the COPYRIGHT SYSTEM the category enabled (term limits, licensing, monopoly gates). The two ε values differ substantially.',
    'This reading anchors on process (the emergence event itself); if referent drifts to the emerged system, the ε would rise sharply post-1710 as the category''s extractive uses become visible. The authoring choice of referent fixes what the story measures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_neutral_referent, conceptual, 'Whether ε measures the emergence process or the emerged copyright system.').

omega_variable(
    statute_of_anne_timing,
    'Did the Statute of Anne (1710 or 1710/1711 depending on calendar) CODIFY an already-coherent common-law category, or did statutory language CONSTITUTE the category in its final form?',
    'Trace legal discourse before and after statute: if ''copyright'' appears in case law and commentary pre-statute with stable meaning, statute codified. If ''copyright'' terminology becomes stable or centralized post-statute, statute constituted.',
    'If codification: common-law emergence is the analytical pivot. If constitution: statute is the real emergence event and common-law development is preparatory. This omega flags the temporal seam between thinkability_reading (emphasizes common-law coherence) and synchronic_diachronic_seam (questions whether emergence is process or state-at-t).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statute_of_anne_timing, empirical, 'Whether statute of Anne codified or constituted copyright as a legal category.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__thinkability_reading, 1680, 1730).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1680, ip_category_emergence__thinkability_reading, theater_ratio, 1680, 0.15).
narrative_ontology:measurement_basis(ip_c_tr_t1680, observed).
narrative_ontology:measurement(ip_c_tr_t1690, ip_category_emergence__thinkability_reading, theater_ratio, 1690, 0.18).
narrative_ontology:measurement_basis(ip_c_tr_t1690, observed).
narrative_ontology:measurement(ip_c_tr_t1700, ip_category_emergence__thinkability_reading, theater_ratio, 1700, 0.22).
narrative_ontology:measurement_basis(ip_c_tr_t1700, observed).
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__thinkability_reading, theater_ratio, 1710, 0.28).
narrative_ontology:measurement_basis(ip_c_tr_t1710, observed).
narrative_ontology:measurement(ip_c_tr_t1720, ip_category_emergence__thinkability_reading, theater_ratio, 1720, 0.32).
narrative_ontology:measurement_basis(ip_c_tr_t1720, observed).
narrative_ontology:measurement(ip_c_tr_t1730, ip_category_emergence__thinkability_reading, theater_ratio, 1730, 0.35).
narrative_ontology:measurement_basis(ip_c_tr_t1730, observed).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1680, ip_category_emergence__thinkability_reading, base_extractiveness, 1680, 0.35).
narrative_ontology:measurement_basis(ip_c_be_t1680, observed).
narrative_ontology:measurement(ip_c_be_t1690, ip_category_emergence__thinkability_reading, base_extractiveness, 1690, 0.42).
narrative_ontology:measurement_basis(ip_c_be_t1690, observed).
narrative_ontology:measurement(ip_c_be_t1700, ip_category_emergence__thinkability_reading, base_extractiveness, 1700, 0.51).
narrative_ontology:measurement_basis(ip_c_be_t1700, observed).
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__thinkability_reading, base_extractiveness, 1710, 0.62).
narrative_ontology:measurement_basis(ip_c_be_t1710, observed).
narrative_ontology:measurement(ip_c_be_t1720, ip_category_emergence__thinkability_reading, base_extractiveness, 1720, 0.68).
narrative_ontology:measurement_basis(ip_c_be_t1720, observed).
narrative_ontology:measurement(ip_c_be_t1730, ip_category_emergence__thinkability_reading, base_extractiveness, 1730, 0.71).
narrative_ontology:measurement_basis(ip_c_be_t1730, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ip_category_emergence__thinkability_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__thinkability_reading, information_standard).
narrative_ontology:boltzmann_floor_override(ip_category_emergence__thinkability_reading, 0.05).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__first_holding_reading).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% This story is the thinkability reading of the ip_category_emergence kernel. The kernel decomposes into three logically distinct but temporally overlapping constraints: (1) thinkability_reading (this story) — the emergence of 'copyright' as a coherent legal concept; (2) first_holding_reading — the shift in legitimate claimant status from guild monopoly to individual author; (3) synchronic_diachronic_seam — the question of whether thinkability and holding are formally independent or are an artifact of temporal framing. Each reading instantiates a different ε, beneficiary structure, and type. They are linked by network.affects_constraints because the coherent category (thinkability) is a necessary condition for the legal framework within which holding claims are made, and because the synchronic/diachronic seam tests whether the three can be separated or collapse into one. All three stories must be read together to understand the full kernel decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
