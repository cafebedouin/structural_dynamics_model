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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: ip_category_emergence__thinkability_reading
 *   human_readable: IP Category Emergence: Thinkability Reading (1710 Coherence Point)
 *   domain: legal/philosophical/historical
 *
 * SUMMARY:
 *   In the century before 1710, disputes over manuscript and book
 *   reproduction were governed by a patchwork of guild privilege, royal
 *   licensing, contractual negotiation, and common practice. No unified legal
 *   vocabulary existed to frame the underlying question: what rights should
 *   govern the reproduction of expression once fixed in a medium? The Statute
 *   of Anne (1710) codified 'copyright' as a distinct, transferable form of
 *   property. This reading claims that 1710 marks the emergence of a new
 *   legal category—'ownable expression'—as thinkable within the legal system.
 *   Before 1710, the disputes existed and were litigated, but they lacked a
 *   coherent conceptual container. After 1710, the same disputes could be
 *   framed as disputes about property in expression, enabling doctrine
 *   development, market formation, and professional practice. This reading
 *   emphasizes the thinkability point (the emergence of coherent conceptual
 *   space) as structurally distinct from the entry of a new class of
 *   claimants (the first-holding reading) or the formal temporal structure of
 *   the threshold (the synchronic_diachronic_seam reading). The constraint is
 *   a reading of the contested kernel 'ip_category_emergence'; it is not
 *   about whether copyright should exist, but about what the emergence of the
 *   copyright category accomplished and when it became thinkable.
 *
 * KEY AGENTS:
 *   - author_as_property_claimant: Pre-1710, the author's claim to the fruits of their labor rested on patronage, licensing, or guild membership; post-1710, the author becomes a property-holder in copyright.
 *   - publishers_and_booksellers: Pre-1710, publishers held revocable privileges; post-1710, publishers acquire coherent property rights in copyright, enabling markets in copy-rights.
 *   - legal_profession: IP law emerges as a distinct practice domain; lawyers, courts, and legislatures acquire a coherent conceptual apparatus that enables consistent dispute resolution.
 *   - unauthorized_copyists: Pre-1710, copying could be challenged on privilege grounds; post-1710, copying becomes an offense against a legal category, prosecutable anywhere the category is recognized.
 *   - pre_1710_dispute_participants: Litigants, scribes, printers, and stationers fighting over book rights before 1710 lacked the vocabulary and doctrinal apparatus that became available in 1710; they shaped the category but were not inside it.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, 0.31).
domain_priors:suppression_score(ip_category_emergence__thinkability_reading, 0.18).
domain_priors:theater_ratio(ip_category_emergence__thinkability_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__thinkability_reading, rope).
narrative_ontology:human_readable(ip_category_emergence__thinkability_reading, "IP Category Emergence: Thinkability Reading (1710 Coherence Point)").
narrative_ontology:topic_domain(ip_category_emergence__thinkability_reading, "legal/philosophical/historical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__thinkability_reading, '35adcf3e-ac3a-4193-8347-bd83154683a1').
narrative_ontology:cs_kernel_codification('35adcf3e-ac3a-4193-8347-bd83154683a1', fixed_text).
narrative_ontology:cs_authority_grounding('35adcf3e-ac3a-4193-8347-bd83154683a1', lineage).
narrative_ontology:cs_interpretation_layer_present('35adcf3e-ac3a-4193-8347-bd83154683a1').
narrative_ontology:cs_reading_relation('35adcf3e-ac3a-4193-8347-bd83154683a1', ip_category_emergence__first_holding_reading, coexists_with).
narrative_ontology:cs_reading_relation('35adcf3e-ac3a-4193-8347-bd83154683a1', ip_category_emergence__synchronic_diachronic_seam, coexists_with).
narrative_ontology:cs_axiom('35adcf3e-ac3a-4193-8347-bd83154683a1', foundational, thinkability_precedes_occupancy).
narrative_ontology:cs_axiom_status(thinkability_precedes_occupancy, holdable).
narrative_ontology:cs_axiom_grounding('35adcf3e-ac3a-4193-8347-bd83154683a1', thinkability_precedes_occupancy, deontological).
narrative_ontology:cs_axiom('35adcf3e-ac3a-4193-8347-bd83154683a1', foundational, categories_constitute_legal_intelligibility).
narrative_ontology:cs_axiom_status(categories_constitute_legal_intelligibility, holdable).
narrative_ontology:cs_axiom_grounding('35adcf3e-ac3a-4193-8347-bd83154683a1', categories_constitute_legal_intelligibility, deontological).
narrative_ontology:cs_reference_frame('35adcf3e-ac3a-4193-8347-bd83154683a1', pre_1710_vocabularic_incoherence).
narrative_ontology:cs_drift_state('35adcf3e-ac3a-4193-8347-bd83154683a1', post_1710_statutory_coherence, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('35adcf3e-ac3a-4193-8347-bd83154683a1', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(ip_category_emergence__thinkability_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, author_as_property_claimant).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, legal_profession).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, publishing_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, publishers_and_booksellers).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, unauthorized_copyists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Before 1710, an author's claim to the fruits of their labor rested on guild membership, licensing privilege, or patronage relationships—personal property or contracted benefit, not a category-based legal entitlement. After 1710, 'copyright' becomes a distinct, transferable, taxable form of property. The author gains a seat at the legal table as a property-holder, not merely as a supplicant to a guild or crown.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, author_as_property_claimant, beneficiary,
    moderate, biographical, constrained, national).

% Publishers inherit legal protection from the new category: they can hold, transfer, and license copyright as a bundle of rights, not as a revocable privilege. The category enables markets in copy-rights themselves, independent of author relationships. They benefit from the coherent property frame as much as authors do.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, publishers_and_booksellers, beneficiary,
    organized, biographical, constrained, national).

% IP law becomes a distinct practice domain. Lawyers, courts, and legislatures acquire a coherent conceptual apparatus ('copyright', 'literary property') that enables consistent dispute resolution and doctrine-building. The category creates careers, precedent lines, and institutional infrastructure.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, legal_profession, beneficiary,
    organized, generational, arbitrage, national).

% Before 1710, copying a manuscript or book was a craft activity or a common practice, legally contested only when guild privileges or specific licensing agreements were in play. After 1710, unauthorized copying becomes an offense against a legal category (copyright), prosecutable anywhere the category is recognized. The copyist's activity becomes categorically forbidden rather than merely privilege-violating.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, unauthorized_copyists, payer,
    powerless, immediate, trapped, local).

% Litigants, scribes, printers, and stationers fighting over manuscript and book rights before 1710 lacked the vocabulary and doctrinal apparatus the thinkability reading claims became available in 1710. They could assert claims but could not frame them as property claims in the newly coherent sense. Their disputes shaped the category; they were not inside it.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, pre_1710_dispute_participants, excluded,
    powerful, biographical, constrained, national).

% The 1710 emergence (Statute of Anne) was an English event. Continental systems developed 'author's rights' frameworks independently and on different schedules. The observer position enables comparison: did the category emerge at different moments, or was 1710 a local instantiation of a broader European thinkability shift?
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, continental_legal_systems, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__thinkability_reading, legal_profession).
narrative_ontology:fixing_cost_class(ip_category_emergence__thinkability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified legal vocabulary ('copyright', 'literary property') that enables all parties—authors, publishers, courts, lawmakers—to frame disputes about expression and reproduction in a common conceptual space. Before 1710, disputes lacked this vocabulary; parties argued about guild privilege, royal grant, or common custom, but not about a distinct category of ownable expression.
% TRANSFER_FUNCTION: Moves authority-to-dispute-resolution from the sphere of guild privilege and royal licensing to the sphere of property law. Also moves the recognized claimant set: pre-1710, only guilds, patrons, and crown agents had standing to contest copying; post-1710, any legal owner of copyright has standing. The category enables the legal system to recognize a new class of claimants.
% ABSENT_VOICES: Unauthorized copyists and scribes who benefit from the pre-1710 ambiguity (the lack of clear legal category means their activities are harder to prosecute). They have no seat in the category-emergence debate itself—the debate is conducted by legal theorists, judges, and publishing-industry advocates who all benefit from coherent IP doctrine. The voices of those who would lose enforcement capability under the new regime are structurally excluded from the founding conversation.
% DISAPPEARANCE_RATIONALE: If the thinkability of 'copyright' as a coherent legal category had never emerged—if disputes continued to be framed in terms of guild privilege, royal grant, and custom rather than property law—the legal system would lack a unified apparatus for governing expression-copying disputes. Markets in copy-rights would not exist; author compensation would remain tied to patronage and licensing rather than property ownership; courts would lack doctrine to develop; the legal profession would not have an IP practice domain. The entire infrastructure of modern copyright law depends on the category emerging and becoming thinkable as property.
% FOUNDING_PROBLEM: Disputes over manuscript and printed-book reproduction were legion by the 17th century: stationers claimed privilege, authors claimed credit, patrons claimed ownership, pirates claimed freedom, courts claimed whatever authority the case at hand seemed to require. The disputes were not resolved; they were managed ad hoc through guild authority, royal decree, and contractual negotiation. No unified legal vocabulary existed to frame the underlying problem: what entitlements should govern the reproduction of expression once it is fixed in a medium?
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (outside the IP law profession itself) document extensive pre-1710 disputes over manuscript and book reproduction: the Stationers' Company battles in England, parallel conflicts over printing privileges across Europe, ongoing tensions between authors' claims and publishers' claims. The founding problem—lack of a coherent legal category—is attested by the very incoherence of pre-1710 doctrine. IP legal scholars also recognize the Statute of Anne (1710) as the codification point where copyright emerges as a distinct category; however, that corroboration comes from within the benefiting professional structure. The independent corroboration is the historical record of pre-1710 disputes lacking a unified vocabulary.
narrative_ontology:disappearance_verdict(ip_category_emergence__thinkability_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__thinkability_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__thinkability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ip_category_emergence__thinkability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__thinkability_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__thinkability_reading_tests).
:- end_tests(ip_category_emergence__thinkability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness measure (0.31 at interval end, 1750) is moderate: the category emergence does solve a genuine coordination problem (unified legal vocabulary for expression disputes enables markets and doctrine), but it also extracts value from copyists and readers who lose the prior ambiguity. The suppression measure (0.18) is low because the category itself does not require coercive enforcement to maintain; enforcement comes later (civil suits, remedies). The theater ratio (0.08) is very low: the category emergence is substantive, not performative. The measurement series shows extractiveness rising sharply from 1650–1710 (0.08 → 0.31), then more gradually to 1750 (0.31 → 0.42)—reflecting the initial coherence jump at 1710 and subsequent doctrine-building and market expansion. The suppression requirement falls over the same interval (0.35 → 0.12), indicating that the category becomes self-enforcing through legal convention and custom rather than requiring active coercion. Theater ratio falls as the category matures (0.22 → 0.05), indicating the copyright system becomes increasingly functional rather than performative.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (author, publisher, legal profession) experience the category emergence as enabling legitimate claims and market activity; their divergence from the payer seats (copyist, prior-dispute participants) is not about the constraint's type—all seats compute it as rope on the structural data—but about whether the coordination function is worth its cost. For the beneficiary, the unified vocabulary enables careers and markets; for the copyist, it forecloses prior ambiguity and turns routine activity into criminality. The legal profession's perspective is analytically unique: lawyers benefit from the category's emergence not because they gain copyright property themselves but because IP law becomes a billable practice domain. The observer seat (continental systems) can compare: did the category emerge at different moments in different jurisdictions, or was the thinkability universal at 1710?
 *
 * DIRECTIONALITY LOGIC:
 *   Author-as-property-claimant and publishers both benefit from the coherent category (d near 0.0, beneficiary end), though for different reasons: authors gain recognition as property-holders, publishers gain markets in copy-rights. The legal profession benefits from the category emerging because it creates a practice domain (d near 0.0, beneficiary end); their directionality is structural rather than extractive—they do not directly capture the copyright rents, but they benefit from the institutional proliferation the category enables. Unauthorized copyists are targeted by the new category: their prior ambiguous status becomes clear illegality (d near 1.0, target end). Pre-1710 dispute participants are excluded rather than targeted: they shaped the category through their disputes but are not seated at the table where the category's post-1710 operation is governed (d intermediate, approaching excluded). The continental observer sits at d = 0.5 (analytical): they have no material stake in any particular jurisdiction's category emergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (lack of a coherent legal vocabulary for expression-copying disputes) remains live as of 1750; copyright doctrine is still developing, courts are still building precedent, and the category is still being refined. The constraint does not show signs of mandatrophy: the category that emerged in 1710 continues to serve its founding function (providing unified vocabulary) and is not atrophying. If the constraint were to show mandatrophy signals, they would appear as theater_ratio rising (enforcement becoming performative rather than functional) or as the category fragmenting back into sub-categories and exceptions that undermine its coherence. The measurements show the opposite: theater declining and extractiveness gradually rising (consistent with legitimate market expansion), not performative maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    thinkability_vs_first_holding,
    'Is category emergence (thinkability) logically and temporally distinct from the entry of a new class of claimants (first-holding)? Or do they collapse into a single phenomenon when examined closely?',
    'Detailed textual analysis of pre-1710 legal disputes and post-1710 doctrine: if disputes pre-1710 could be restated in IP vocabulary but were not, thinkability and first-holding are distinct; if the emergence of the category and the emergence of author-as-claimant are simultaneous and inseparable, they collapse to one phenomenon.',
    'If distinct: the thinkability reading stands as a separate constraint; if collapsed: the kernel reduces to a single-moment dual-function transition (category emergence = claimant-set expansion), and sibling readings merge. This is the M4/M5 collapse test the synchronic_diachronic_seam reading investigates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(thinkability_vs_first_holding, conceptual, 'Whether thinkability and first-holding are structurally independent or a single phenomenon.').

omega_variable(
    continental_emergence_timing,
    'Did the thinkability of ''copyright'' as a legal category emerge at 1710 (English Statute of Anne), or did it emerge earlier or later or differently across European jurisdictions?',
    'Comparative legal history: examine pre-1710 and post-1710 legal language in England, France, the German territories, and other jurisdictions; map the emergence point for each system.',
    'If emergence is universal at 1710, the reading''s referent is a genuine threshold. If emergence is staggered or jurisdiction-specific, the reading''s referent (1710 coherence) is local rather than universal, and the extracted value from the category emergence varies by system.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(continental_emergence_timing, empirical, 'Whether IP category emergence was a universal 1710 threshold or a staggered, jurisdiction-specific process.').

omega_variable(
    prior_vocabulary_continuity,
    'Did 1710 category emergence introduce genuinely new vocabulary (''copyright'', ''literary property'') or did it coherently frame pre-existing but incoherent concepts that were already in use in disputes?',
    'Lexical and conceptual history: trace the terms and their usage pre-1710 and post-1710; determine whether coherence came from new words or from organizing existing terms into a unified framework.',
    'If genuinely new vocabulary: the category emergence is a conceptual leap, supporting the rope reading (real coordination function). If reframing of existing terms: the thinkability reading may understate the prior disputability, and extraction may be higher than measured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prior_vocabulary_continuity, empirical, 'Whether 1710 introduced new vocabulary or organized existing terms into coherence.').

omega_variable(
    reading_kernel_framing,
    'Does this reading''s framing—category emergence as a thinkability point in conceptual space, distinct from occupancy change (first-holding) or formal temporal structure (synchronic_diachronic_seam)—reflect the historical record or impose a philosophical commitment to ''thinkability'' that shapes what the historical record looks like?',
    'Reflexive analysis: examine this reading''s own axioms and reference frame; determine whether the category-emergence narrative depends on accepting ''thinkability as primary'' as a foundational premise, and whether that premise is supported by evidence independent of the reading''s own theoretical commitments.',
    'If the reading''s framing is supported by independent evidence: it stands as a coherent reading. If the framing depends on accepting thinkability-primacy without external corroboration: the reading is self-confirming, and alternative framings (first-holding, synchronic_diachronic_seam) should receive equal weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_framing, conceptual, 'Whether this reading''s thinkability-first framework reflects the historical record or imposes a philosophical commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__thinkability_reading, 1650, 1750).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1650, ip_category_emergence__thinkability_reading, theater_ratio, 1650, 0.22).
narrative_ontology:measurement(ip_c_tr_t1680, ip_category_emergence__thinkability_reading, theater_ratio, 1680, 0.18).
narrative_ontology:measurement(ip_c_tr_t1700, ip_category_emergence__thinkability_reading, theater_ratio, 1700, 0.12).
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__thinkability_reading, theater_ratio, 1710, 0.08).
narrative_ontology:measurement(ip_c_tr_t1730, ip_category_emergence__thinkability_reading, theater_ratio, 1730, 0.06).
narrative_ontology:measurement(ip_c_tr_t1750, ip_category_emergence__thinkability_reading, theater_ratio, 1750, 0.05).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1650, ip_category_emergence__thinkability_reading, base_extractiveness, 1650, 0.08).
narrative_ontology:measurement(ip_c_be_t1680, ip_category_emergence__thinkability_reading, base_extractiveness, 1680, 0.14).
narrative_ontology:measurement(ip_c_be_t1700, ip_category_emergence__thinkability_reading, base_extractiveness, 1700, 0.21).
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__thinkability_reading, base_extractiveness, 1710, 0.31).
narrative_ontology:measurement(ip_c_be_t1730, ip_category_emergence__thinkability_reading, base_extractiveness, 1730, 0.38).
narrative_ontology:measurement(ip_c_be_t1750, ip_category_emergence__thinkability_reading, base_extractiveness, 1750, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1650, ip_category_emergence__thinkability_reading, suppression_requirement, 1650, 0.35).
narrative_ontology:measurement(ip_c_su_t1680, ip_category_emergence__thinkability_reading, suppression_requirement, 1680, 0.29).
narrative_ontology:measurement(ip_c_su_t1700, ip_category_emergence__thinkability_reading, suppression_requirement, 1700, 0.23).
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__thinkability_reading, suppression_requirement, 1710, 0.18).
narrative_ontology:measurement(ip_c_su_t1730, ip_category_emergence__thinkability_reading, suppression_requirement, 1730, 0.15).
narrative_ontology:measurement(ip_c_su_t1750, ip_category_emergence__thinkability_reading, suppression_requirement, 1750, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__thinkability_reading, information_standard).
narrative_ontology:boltzmann_floor_override(ip_category_emergence__thinkability_reading, 0.05).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__first_holding_reading).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'ip_category_emergence'. The kernel captures the historical moment of 1710 when copyright emerged as a coherent legal category in English law. The thinkability_reading emphasizes the emergence of a new conceptual space (ownable expression as thinkable); the first_holding_reading emphasizes the entry of authors as a new class of claimants; the synchronic_diachronic_seam reading questions whether these are structurally distinct or a single phenomenon. Each reading instantiates a different constraint with different ε values and beneficiary/victim structures. All three readings share the historical referent (1710, Statute of Anne) but interpret its significance differently. The thinkability_reading claims that 1710 marks the emergence of a unified legal vocabulary that enables all subsequent IP doctrine; the first_holding_reading claims that 1710 marks the entry of authors into the legal system as legitimate claimants; the synchronic_diachronic_seam reading questions whether this distinction is real or a temporal framing artifact. This story links to both siblings via network.affects_constraints; each sibling links back, forming a three-story constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
