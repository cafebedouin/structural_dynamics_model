% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__institutional_reallocation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__institutional_reallocation_reading, []).

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
 *   constraint_id: statute_of_anne_ip_foundation__institutional_reallocation_reading
 *   human_readable: Statute of Anne IP Foundation—Institutional Reallocation Reading
 *   domain: legal/institutional/intellectual-property
 *
 * SUMMARY:
 *   The Statute of Anne (1710) is read here as an institutional
 *   reallocation—a restructuring of who holds printing rights within the
 *   institutional ecosystem. Under the Stationers' monopoly, the company held
 *   the right to license printing as a corporate body backed by Crown
 *   charter. The statute moved that right to individual authors (and their
 *   assignees, typically publishers) for a limited statutory term. This is a
 *   reallocation of pre-existing institutional control, not the creation of a
 *   new conceptual category from nothing. The statute is claimed as
 *   tangled_rope because it simultaneously coordinates author incentives and
 *   extraction from the Stationers' monopoly while extracting monopoly prices
 *   from readers during the statutory term. The founding problem is live—the
 *   inefficiency of the old monopoly persists as a normative grievance—but
 *   the institutional form has shifted from a single chartered gatekeeper to
 *   distributed property rights.
 *
 * KEY AGENTS:
 *   - Stationers' Company: institutional monopolist, charter holder, loses gatekeeping authority (power=institutional, exit=trapped)
 *   - Authors: property-right holders, moderate power, can assign rights to publishers (power=moderate, exit=arbitrage)
 *   - Book publishers: institutional beneficiaries, assignees of author rights, inherit enforcement role (power=institutional, exit=mobile)
 *   - Prospective printers/booksellers: enabled to enter market but constrained by title-by-title rights (power=moderate, exit=mobile)
 *   - Readers: gain diversity, bear monopoly pricing during term, benefit after term expiration (power=powerless, exit=constrained)
 *   - Parliament/Crown: agenda setter, exercises sovereignty to restructure property regime (power=institutional, exit=analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.62).
domain_priors:suppression_score(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.41).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__institutional_reallocation_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__institutional_reallocation_reading, "Statute of Anne IP Foundation—Institutional Reallocation Reading").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__institutional_reallocation_reading, "legal/institutional/intellectual-property").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__institutional_reallocation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'a0636dd5-1c3c-43a9-af46-64d182d03349').
narrative_ontology:cs_kernel_codification('a0636dd5-1c3c-43a9-af46-64d182d03349', formalized).
narrative_ontology:cs_authority_grounding('a0636dd5-1c3c-43a9-af46-64d182d03349', extraction).
narrative_ontology:cs_interpretation_layer_present('a0636dd5-1c3c-43a9-af46-64d182d03349').
narrative_ontology:cs_reading_relation('a0636dd5-1c3c-43a9-af46-64d182d03349', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('a0636dd5-1c3c-43a9-af46-64d182d03349', statute_of_anne_ip_foundation__entangled_event_reading, coexists_with).
narrative_ontology:cs_axiom('a0636dd5-1c3c-43a9-af46-64d182d03349', foundational, institutional_reallocation_precedent).
narrative_ontology:cs_axiom_status(institutional_reallocation_precedent, holdable).
narrative_ontology:cs_axiom_grounding('a0636dd5-1c3c-43a9-af46-64d182d03349', institutional_reallocation_precedent, conventional).
narrative_ontology:cs_axiom('a0636dd5-1c3c-43a9-af46-64d182d03349', foundational, author_property_holder_status).
narrative_ontology:cs_axiom_status(author_property_holder_status, holdable).
narrative_ontology:cs_axiom_grounding('a0636dd5-1c3c-43a9-af46-64d182d03349', author_property_holder_status, deontological).
narrative_ontology:cs_reference_frame('a0636dd5-1c3c-43a9-af46-64d182d03349', monopoly_gatekeeping_authority).
narrative_ontology:cs_drift_state('a0636dd5-1c3c-43a9-af46-64d182d03349', post_statute_maturation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a0636dd5-1c3c-43a9-af46-64d182d03349', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, book_publishers).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors_as_property_holders).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_monopoly).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, prospective_printers_and_booksellers).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, readers).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Stationers' Company held a chartered monopoly on book production and distribution, controlling entry and pricing through licensing and guild membership. The statute stripped this monopoly by redirecting author-held printing rights directly to individual authors and assignees, displacing the Company's institutional gatekeeping role. The Company could not exit—its entire charter was predicated on controlling the supply of books, and the statute's enforcement mechanism (author suit, statutory damages) made the old monopoly structurally incompatible with the new regime.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_monopoly, payer,
    institutional, generational, trapped, national).

% Gained the statutory right to control printing of their works for a limited term (14 years renewable once). This was a reallocation of pre-existing institutional rights—the Stationers had held those rights through monopoly grant. Authors could now assign rights to publishers or sell copies directly. The benefit was contingent on enforcement: the statute created a cause of action for authors against unauthorized printers, backed by statutory damages and court process.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors_as_property_holders, beneficiary,
    moderate, biographical, arbitrage, national).

% As the principal recipients of author assignments (most authors could not enforce rights themselves), publishers inherited the enforcement role and benefited from monopoly pricing rights on individual titles. They operated within the new institutional space that the statute created—one where rights holders competed on individual works rather than via charter monopoly. Entry was no longer restricted by guild membership, but control of capital and distribution networks persisted.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, book_publishers, beneficiary,
    institutional, generational, mobile, national).

% Could now enter the printing and bookselling trades without Stationers' license. The statute broke the guild's gatekeeper role, enabling competitive entry. However, they faced title-by-title restraint: each work was protected by author or assignee rights, so they could not print anything with impunity—they faced a more distributed but equally binding set of property claims.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, prospective_printers_and_booksellers, beneficiary,
    moderate, biographical, mobile, national).

% Experienced increased title diversity and availability (competition reduced gatekeeping inefficiency) but paid more during the statutory term when rights holders enforced monopoly pricing. After term expiration, works entered the commons and could be reprinted cheaply—a delayed benefit. Readers had no seat in the statute's legislative process and no enforcement role.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, readers, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__institutional_reallocation_reading, readers, payer).

% Enacted the statute as an exercise of sovereign authority, restructuring intellectual property from a monopoly grant to a time-limited property right. The Crown's benefit was normative (a regulated literary marketplace) rather than fiscal; the statute did not create a crown revenue stream equivalent to the monopoly licensing fees the Crown had previously extracted from the Stationers. The Crown remained the ultimate authority that could revise or sunset the statutory regime.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, crown_parliament, agenda_setter,
    institutional, generational, analytical, national).

% A non-agent entity: the legal doctrine that Parliament's authority to regulate commerce and property supersedes chartered corporate monopolies. The statute vindicated parliamentary oversight and public interest intervention in market structure.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, legal_tradition_of_parliamentary_sovereignty, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(statute_of_anne_ip_foundation__institutional_reallocation_reading, legal_tradition_of_parliamentary_sovereignty).

% Printers, pressworkers, and apprentices were subject to whoever employed them—either continuing Stationers members or new entrant publishers. The statute did not empower labor; it restructured ownership of rights. Wages and working conditions remained determined by employer power, not by the statute. Apprentices still faced guild-style indenture where it persisted.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, unorganized_printing_workforce, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__institutional_reallocation_reading, book_publishers).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__institutional_reallocation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Redirects institutional control of book production from a chartered monopoly (Stationers' Company) to a distributed property-right system where individual authors and their assignees hold time-limited exclusive printing rights. Solves the coordination problem of how to allocate incentives to authors without perpetuating a single corporate gatekeeper.
% TRANSFER_FUNCTION: Moves the institutional authority to license printing from the Crown (via Stationers' charter) to individual authors and their assignees (via statutory right). Printers and readers transfer payment to rights holders during the statutory term, then the works enter the commons. Publishers capture much of this transfer through assignment of author rights.
% ABSENT_VOICES: Printing workers, copyists, and the unorganized literary public. Printers' apprentices and journeymen had no seat at the table; the statute restructured their employment conditions but they were not parties to the legislative debate. Readers and competing printers would have contested the length and scope of the monopoly term, but they lacked political standing.
% DISAPPEARANCE_RATIONALE: If the statute were repealed overnight, the Stationers' charter would remain intact unless separately revoked—the institutional space would revert to monopoly gatekeeping unless replaced by an alternative regime. Book production would reorganize around the restored licensing authority; the incentive structure for authorship would shift back toward patronage and guild-affiliated publication.
% FOUNDING_PROBLEM: The Stationers' monopoly created bottlenecks in book distribution, suppressed entry of new printers, and left authors with minimal control or compensation for their work. Copyright had no separate institutional existence—it was subsumed in the monopoly grant. The founding problem was the gap between author interests and the Stationers' incentive to maximize their own revenues.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary parliamentary debates (1710) and petitions from authors and prospective printers attest that the monopoly was extractive and inefficient. Stationers' own records show they collected substantial licensing revenue while compensating authors minimally or not at all. Independent scholars of the period (Loewenstein, Rose) analyze the monopoly's economic inefficiency outside the benefiting parties.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__institutional_reallocation_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__institutional_reallocation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__institutional_reallocation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statute_of_anne_ip_foundation__institutional_reallocation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 at interval end) because the statute creates distributed monopoly control—readers pay higher prices during the statutory term for each title, and publishers (as assignees) capture much of that extraction. This is extraction from readers, not pure coordination of author incentives. Suppression is moderate (0.41) because enforcement relies on lawsuit by authors/assignees against unauthorized printers; there is no centralized enforcement apparatus like the Stationers' inspection and destruction authority. Theater is low (0.22): the statute's stated purpose (encouraging learning and authorship) is largely accomplished by the actual mechanism (time-limited property rights with public domain reversion). The constraint is tangled_rope: it genuinely coordinates author incentives (coordination function) while extracting monopoly surplus from readers during the term (asymmetric extraction), and it requires active enforcement (statutory damages, author litigation). The beneficiaries are publishers (via assignment) and authors (property holders); the victim is the Stationers' monopoly (institutional displacement). Measurements show slight rise in extractiveness early (0–21 years as the new regime solidifies and publishers optimize pricing strategies) then stabilization, suggesting the underlying institutional equilibrium settles in. Suppression remains flat because litigation-based enforcement does not intensify—it reaches a steady state where publishers and authors routinely sue unauthorized printers.
 *
 * PERSPECTIVAL GAP:
 *   The Stationers' Company seats and the author/publisher seats will compute different constraint types from the same structural data. From the Stationers' perspective, the statute is a snare—their monopoly was stripped without compensation, they are trapped in the old regime by the now-obsolete charter, and enforcement actively excludes their licensing authority. From the publisher/author perspective, it is a coordination mechanism with real benefits (incentives for authorship) that happens to extract from readers. From the reader perspective, it is extraction moderated by eventual commons reversion (a temporary snare). The engine computes these divergences from the structural data—directionality differs by seat because power, exit options, and beneficiary status differ. The authored claim (tangled_rope) reflects the institutional aggregation: the regime as a whole coordinates and extracts simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   The Stationers' Company (trapped, institutional power) is a high-d target (d near 1.0): they are structurally displaced, lose gatekeeping revenue, and cannot exit because their entire charter was predicated on printing control. Authors (moderate power, arbitrage exit) have low d: they gain property rights and can assign them, so they benefit. Publishers (institutional power, mobile exit) have low d: they can choose whether to acquire author rights and which titles to publish; they benefit from the new regime. New printers (moderate power, mobile exit) have moderate d: they gain entry rights but face title-by-title restraint, a more diffuse constraint than the monopoly but still limiting. Readers (powerless, constrained exit) have moderate-high d: they gain diversity but pay higher prices during the statutory term. The Crown/Parliament (analytical seat) sets the agenda and can exit by revising the statute—d near 0.0 (structural beneficiary of the coordination function, though not a direct financial beneficiary).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Stationers' monopoly inefficiency) remains live throughout the interval—the statute does not solve the problem, it displaces it. The new regime still creates monopoly pricing (now distributed across individual titles rather than centralized), so the inefficiency persists in a different form. The statutory sunset (14-year renewable term) is designed to manage this: works enter the commons after the term, limiting the duration of monopoly extraction. The statute's mandate—to encourage learning and authorship—is partially satisfied by the actual mechanism (time-limited property rights do create incentives), but the extraction component (monopoly pricing) is a side effect, not the mandate. This is a textbook tangled_rope: genuine coordination of incentives (mandate-aligned) entangled with structural extraction (unintended or justified as the price of incentive alignment). The classification prevents misreading it as pure extraction (snare) or pure coordination (rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_displacement_vs_conceptual_novelty,
    'Is the statute primarily an institutional reallocation of pre-existing rights (from Stationers to authors/publishers) or did it instantiate a conceptually new category (limited-term copyright distinct from perpetual property)?',
    'Genealogical textual analysis of the statute''s preamble, parliamentary debates, and legal reasoning by contemporary jurists. If the statute uses language of ''reallocation'' and ''authors'' rights'' (pre-existing category language) rather than ''invention'' and ''new property'' (conceptual novelty language), the institutional reading holds. If the text emphasizes learning, limited term, and public interest (regulatory novelty), the conceptual reading holds.',
    'If reallocation, the constraint is primarily a restructuring of institutional actors and their relative power; extraction is a side effect of the new distribution. If conceptually novel, the constraint is the birth of a new regulatory form, and the institutional reallocation is instrumental to it. The classification would not change, but the narrative framing and the diagnosis of what the statute solves would shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_displacement_vs_conceptual_novelty, empirical, 'Whether the statute is institutional displacement or conceptual invention.').

omega_variable(
    publisher_capture_of_author_rights,
    'To what extent did publishers capture author-held rights through assignment, and was this capture a foreseen consequence or an emergent abuse of the statutory mechanism?',
    'Historical record of author-publisher agreements, assignment practices, and litigation over terms. If most authors assigned rights voluntarily to publishers (low-power bargaining aside), and publishers used assignment as a standard practice, capture was foreseen and structural. If assignment emerged gradually and authors resisted, capture was emergent.',
    'High capture means the statute''s principal beneficiary is institutional publishers, not individual authors, despite the text''s framing. This would suggest the constraint''s actual operation is closer to rent transfer (from monopoly to publishers) than incentive creation. Low capture means individual authors retained meaningful control, making the incentive story credible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publisher_capture_of_author_rights, empirical, 'Whether the statute''s author-rights provisions were co-opted by publishers.').

omega_variable(
    statutory_enforcement_realism,
    'How effective was the statutory enforcement mechanism (author lawsuits, statutory damages) relative to the Stationers'' enforcement apparatus (inspections, seizures, guild discipline)?',
    'Quantitative record of litigation cases, damage awards, and prosecution rates; comparison of enforcement burden on authors vs. the Stationers'' institutional resources.',
    'Weak enforcement means the statutory mechanism is theatrical—the rights exist on paper but are not enforced, so extraction does not materialize and readers benefit from increased copying. Strong enforcement means extraction is real and suppression is higher than authored (0.41). The suppression metric assumes moderate enforcement; this omega tests whether that assumption holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(statutory_enforcement_realism, empirical, 'The credibility of statutory enforcement relative to institutional enforcement.').

omega_variable(
    stationers_exit_path_and_agency_loss,
    'Could the Stationers'' Company have adapted to the new regime (e.g., by becoming a publishers'' guild or licensing collective) or were they structurally foreclosed by the statute''s design?',
    'Historical record of what the Stationers actually attempted post-1710 and what legal/market barriers they faced. If they formed a publishers'' society or became prominent in the new market, exit was available. If they attempted adaptation and were blocked, exit was trapped.',
    'Available exit means the Stationers'' role as victim is softer than authored (they could have transformed). Trapped exit (what the narrative suggests) means the victim role is sharp—institutional obsolescence is structural, not strategic. The classification remains tangled_rope either way, but the mandatrophy story shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stationers_exit_path_and_agency_loss, empirical, 'Whether the Stationers'' institutional collapse was inevitable or chosen.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t7, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 7, 0.19).
narrative_ontology:measurement_basis(stat_tr_t7, observed).
narrative_ontology:measurement(stat_tr_t14, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 14, 0.21).
narrative_ontology:measurement_basis(stat_tr_t14, observed).
narrative_ontology:measurement(stat_tr_t21, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 21, 0.23).
narrative_ontology:measurement_basis(stat_tr_t21, observed).
narrative_ontology:measurement(stat_tr_t35, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 35, 0.22).
narrative_ontology:measurement_basis(stat_tr_t35, observed).
narrative_ontology:measurement(stat_tr_t50, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement_basis(stat_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t7, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 7, 0.58).
narrative_ontology:measurement_basis(stat_be_t7, observed).
narrative_ontology:measurement(stat_be_t14, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 14, 0.61).
narrative_ontology:measurement_basis(stat_be_t14, observed).
narrative_ontology:measurement(stat_be_t21, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 21, 0.63).
narrative_ontology:measurement_basis(stat_be_t21, observed).
narrative_ontology:measurement(stat_be_t35, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 35, 0.62).
narrative_ontology:measurement_basis(stat_be_t35, observed).
narrative_ontology:measurement(stat_be_t50, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(stat_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t7, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 7, 0.4).
narrative_ontology:measurement_basis(stat_su_t7, observed).
narrative_ontology:measurement(stat_su_t14, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 14, 0.41).
narrative_ontology:measurement_basis(stat_su_t14, observed).
narrative_ontology:measurement(stat_su_t21, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 21, 0.42).
narrative_ontology:measurement_basis(stat_su_t21, observed).
narrative_ontology:measurement(stat_su_t35, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 35, 0.41).
narrative_ontology:measurement_basis(stat_su_t35, observed).
narrative_ontology:measurement(stat_su_t50, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 50, 0.41).
narrative_ontology:measurement_basis(stat_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__institutional_reallocation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.12).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__entangled_event_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel: the Statute of Anne's role in founding intellectual property. The institutional_reallocation_reading emphasizes the reallocation of control from Stationers to authors/publishers; the conceptual_emergence_reading emphasizes the birth of limited-term copyright as a regulatory category; the entangled_event_reading denies separability. All three share the same historical event but disagree on its structural meaning. ε values differ because the referent differs under each reading's own lights: institutional reallocation reading measures the extraction inherent in the new distribution; conceptual reading measures the conceptual innovation itself (lower extraction when the coordinate benefit is emphasized); entangled reading refutes the decomposition. Network edges link the readings as alternative framings of one kernel, not as causal dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statute_of_anne_ip_foundation__institutional_reallocation_reading, institutional, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
