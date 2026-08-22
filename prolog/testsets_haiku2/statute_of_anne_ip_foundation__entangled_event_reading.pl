% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__entangled_event_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__entangled_event_reading, []).

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
 *   constraint_id: statute_of_anne_ip_foundation__entangled_event_reading
 *   human_readable: Statute of Anne IP Foundation (Entangled Event Reading)
 *   domain: legal/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne (1710) is read here as a single, undecomposable event
 *   in which conceptual and institutional change are simultaneous and
 *   mutually constitutive. The statute is not 'first' introducing an
 *   already-thinkable concept of copyright into law, nor is it 'merely'
 *   reallocating existing rights between institutions. Rather, the act of
 *   codifying a time-limited monopoly as authored property IS the act of
 *   making 'copyright' thinkable as a category. The reading denies the clean
 *   separation that other readings (conceptual_emergence_reading,
 *   institutional_reallocation_reading) attempt to achieve. This entanglement
 *   means the constraint cannot be classified by looking at either its
 *   conceptual or institutional dimension alone—it is a tangled rope because
 *   the coordination function (stabilizing monopoly, enabling authorship) and
 *   the extraction function (transferring reader access, formalizing what was
 *   informal) are created in the same event and cannot be disentangled
 *   without destroying what makes the constraint cohere.
 *
 * KEY AGENTS:
 *   - book publishers: organized, seeking legal security for monopoly; practical beneficiaries of the statute's author-to-publisher assignment contracts
 *   - nascent author profession: powerless, nominally named as rights-holders but immediately bound to transfer rights to publishers
 *   - Stationers' Company: organized, losing perpetual monopoly but forced to adapt to new legal regime
 *   - readers and scholars: powerless, bearing the cost of restricted copying under a new legal regime rather than informal monopoly
 *   - Parliament: institutional agenda-setter, framing monopoly as incentive and authorship as property
 *   - intellectual commons: excluded non-agent, losing territory to codified monopoly presumption
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__entangled_event_reading, 0.62).
domain_priors:suppression_score(statute_of_anne_ip_foundation__entangled_event_reading, 0.41).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__entangled_event_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__entangled_event_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__entangled_event_reading, "Statute of Anne IP Foundation (Entangled Event Reading)").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__entangled_event_reading, "legal/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__entangled_event_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__entangled_event_reading, '96890a08-e8a2-4a63-ae47-7d7ea626b401').
narrative_ontology:cs_kernel_codification('96890a08-e8a2-4a63-ae47-7d7ea626b401', fixed_text).
narrative_ontology:cs_authority_grounding('96890a08-e8a2-4a63-ae47-7d7ea626b401', extraction).
narrative_ontology:cs_interpretation_layer_present('96890a08-e8a2-4a63-ae47-7d7ea626b401').
narrative_ontology:cs_reading_relation('96890a08-e8a2-4a63-ae47-7d7ea626b401', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('96890a08-e8a2-4a63-ae47-7d7ea626b401', statute_of_anne_ip_foundation__institutional_reallocation_reading, influences).
narrative_ontology:cs_axiom('96890a08-e8a2-4a63-ae47-7d7ea626b401', foundational, concept_and_institution_inseparable).
narrative_ontology:cs_axiom_status(concept_and_institution_inseparable, holdable).
narrative_ontology:cs_axiom_grounding('96890a08-e8a2-4a63-ae47-7d7ea626b401', concept_and_institution_inseparable, conventional).
narrative_ontology:cs_axiom('96890a08-e8a2-4a63-ae47-7d7ea626b401', secondary, copyright_emerges_as_legal_category_through_statute).
narrative_ontology:cs_axiom_status(copyright_emerges_as_legal_category_through_statute, holdable).
narrative_ontology:cs_axiom_grounding('96890a08-e8a2-4a63-ae47-7d7ea626b401', copyright_emerges_as_legal_category_through_statute, deontological).
narrative_ontology:cs_reference_frame('96890a08-e8a2-4a63-ae47-7d7ea626b401', informal_monopoly_and_absence_of_copyright_concept).
narrative_ontology:cs_drift_state('96890a08-e8a2-4a63-ae47-7d7ea626b401', post_statute_1770_consolidation, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('96890a08-e8a2-4a63-ae47-7d7ea626b401', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, book_publishers).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, nascent_author_profession).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, conceptual_clarity).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, prior_commons_access).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, stationers_company).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, readers_and_scholars).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__entangled_event_reading, limited_monopoly_doctrine).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__entangled_event_reading, author_natural_rights_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publishers (and surviving members of the Stationers' Company) are the practical beneficiaries of the statute. They negotiate with authors to assign the new copyright right, draft standard-form contracts that transfer the right for modest flat fees, and collect monopoly rents on published works. The statute replaces the old system (informal Stationers' monopoly, royal privilege grants) with a legal framework that is clearer, more enforceable, and extends monopoly protection across the kingdom uniformly. Publishers gain from both the legal clarity and the nominal attribution of rights to authors—they can now say they are enforcing 'authors' property' rather than their own monopoly privilege.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, book_publishers, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__entangled_event_reading, book_publishers, agenda_setter).

% The statute nominally creates the category 'author' as a legal rights-holder, enabling writers to claim standing in the distribution chain and potentially negotiate royalties or favorable assignment terms. However, individual authors face publisher-drafted contracts that immediately transfer the right for fixed fees, and the nascent author profession has no collective power to resist standardized terms. The category 'author' is created by the statute and cannot be inhabited outside it—to be an author in law is to be bound by the copyright framework. Exit options are constrained (before the statute, writers had no legal standing; after it, they have legal standing only within the copyright system) and identity-locked (the identity 'author' is constituted by the statute's legal frame, not by pre-existing writerly practices).
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, nascent_author_profession, beneficiary,
    powerless, biographical, identity_locked, national).

% The Stationers' Company held a perpetual, unchallengeable monopoly by royal charter before 1710. The statute converts this into a time-limited monopoly nominally held by individual authors but practically administered by publishers. The company faces a transition: members can adapt by acquiring author-rights through assignment contracts (many did), but the company as an institution loses its statutory monopoly and must reorganize around the new legal regime. The company is trapped in the sense that it cannot exit the statute's framework—the old monopoly is no longer defensible once the statute is in place—but organized actors within the company adapt by capturing the new legal mechanism.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, stationers_company, payer,
    organized, generational, trapped, national).

% Readers encounter a new legal regime where copying published books is legally prohibited for 14 years (plus 14 if the author survives). Before the statute, copying could be suppressed only through the Stationers' Company's informal monopoly, which was legally unchallengeable but practically porous—unlicensed printers, pirated editions, and informal copying were common. The statute makes monopoly explicit and legal, with statutory penalties for infringement. Readers are trapped because they face a more explicit, better-enforced restriction on copying; their exit options are limited to purchasing authorized copies or waiting for the monopoly to expire. They bear the cost of restricted copying and higher prices but gain the abstract promise that monopoly will eventually expire (unlike the old perpetual monopoly).
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, readers_and_scholars, payer,
    powerless, biographical, trapped, national).

% The conceptual and institutional space of 'knowledge available without legal restriction' loses territory to the new copyright regime. Before the statute, published works were subject to informal monopoly (the Stationers' Company's market control) but had no statutory copyright status—once a work was published and distributed, copying could not be legally prohibited. The statute changes this by creating a legal presumption of monopoly for published works: copying is now legally prohibited for 14-28 years (depending on author's survival), and must be affirmatively permitted (through fair use, scholarly quotation, or public domain status after monopoly expires). The commons is not erased, but it becomes a space that must be explicitly defended against copyright's default presumption.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, intellectual_commons, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(statute_of_anne_ip_foundation__entangled_event_reading, intellectual_commons).

% Parliament acts as the authority that codifies the new arrangement, translating a political settlement between publishers and the Crown into statutory law. Parliament frames the statute as an incentive to learning and authorship ('for the Encouragement of Learning'), positioning it as a legitimate public policy rather than a favor to the book trade. In this reading, Parliament's role is crucial because the act of codifying the settlement is the act that makes 'copyright' thinkable as a legal concept—there is no pre-existing concept waiting to be instituted; the statute's language and legal form create the concept.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, parliament, agenda_setter,
    institutional, generational, analytical, national).

% Printers and booksellers who operated outside the Stationers' Company's monopoly (by printing in Scotland, Ireland, or through smuggling) face new legal restrictions under the statute. Before 1710, they could defend unauthorized reprints by arguing that the Stationers' Company's monopoly was a corporate privilege, not a legal right. The statute creates a legal right in authors (and their assignees) to suppress competing editions, giving publishers statutory standing to pursue legal action against unlicensed competition. These actors are excluded from the statute's beneficiary frame but are directly targeted by its enforcement machinery.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, unlicensed_printers_and_pirates, excluded,
    moderate, biographical, trapped, national).

% Continental European legal theorists and jurisdictions develop copyright frameworks grounded in author's natural rights (droit d'auteur tradition), in parallel and sometimes in reaction to the English statute. England's framing of copyright as a monopoly privilege (rather than as an author's natural property right) creates an interpretive and legal distance between English and continental systems. This distance constrains how the statute can be adopted, translated, or harmonized internationally, and contributes to divergent copyright traditions in civil-law jurisdictions.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, continental_legal_tradition, excluded,
    institutional, civilizational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__entangled_event_reading, book_publishers).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__entangled_event_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The statute solves a coordination problem for the legal regulation of monopoly: it converts an informal, legally-fragile monopoly (the Stationers' Company's market dominance, backed by royal privilege but lacking statutory clarity) into a written, statutory regime that extends monopoly protection to all publishers and authors while setting an explicit time limit. This creates predictable legal standing for monopoly enforcement across England without requiring individual royal privilege grants. The statute also creates the legal categories ('author,' 'copyright,' 'limited monopoly') that enable market transactions between authors and publishers that did not exist before.
% TRANSFER_FUNCTION: The statute transfers the exclusive right to copy published books from the Stationers' Company (and the Crown's discretionary grant of monopoly) to authors nominally, and to publishers practically through assignment contracts. The arrangement moves valuable monopoly rents from informal monopoly enforcement to formalized, legal monopoly rents collected by publishers who contract with authors. It also transfers from readers and scholars the ability to copy texts freely—converting an informal, contestable monopoly (the Stationers' Company's market control) into an explicit legal prohibition (copyright infringement). The statute moves from 'monopoly as enforced practice' to 'monopoly as codified law,' making the extraction more visible and more legally defensible.
% ABSENT_VOICES: Continental legal traditions (which would articulate copyright as author's natural rights, not monopoly privilege) remain outside the English institutional frame and have no voice in the statute's negotiation. Readers and scholarly communities, who benefit from the prior commons access and face new restrictions, are not in the room—the statute is negotiated between Parliament, the Crown, and organized publishers seeking legal security. Working authors outside the publishing trade (journalists, pamphleteers, scribal authors) have no collective voice; individual writing professionals are the nominal beneficiaries but not agenda-setters, and have no power to resist publisher-drafted assignment contracts.
% DISAPPEARANCE_RATIONALE: If the statute vanished, the Stationers' Company's informal monopoly would persist (or require renegotiation with the Crown), but without statutory clarity or renewal mechanisms. The legal category of 'copyright' would not exist—the right would remain a monopoly privilege granted at the Crown's discretion, not a property-like entitlement to an author or publisher. Publishers would lose the legal standing and enforcement machinery the statute created. The entire subsequent development of copyright law globally—which takes the statute's framing as its foundation—would be foreclosed or radically altered. Readers would regain effective access to copying and reprinting that the statute's enforcement prevented.
% FOUNDING_PROBLEM: The Stationers' Company held a perpetual monopoly on printing and selling books, granted and renewed by royal charter. This monopoly was under pressure from unlicensed printers, pirates operating from Scotland and Ireland, and intellectual currents questioning perpetual privileges and monopolies. Parliament sought to stabilize the monopoly for legitimate publishers while appearing to incentivize learning and authorship (rather than merely rewarding a corporate monopoly). The statute achieves this by creating a limited, renewable monopoly framed as an author's property right and an incentive to learning, rather than as a corporate privilege.
% FOUNDING_PROBLEM_CORROBORATION: Parliamentary petitions from publishers (Stationers' Company members and independent booksellers) document lobbying for security against piracy and unlicensed competition, preserved in parliamentary archives. The statute's preamble itself states the purpose: 'for the Encouragement of Learning, the Printer or other Person who laid out the Money and the Hazard in Printing or Reprinting any Book shall have the Sole Liberty.' Independent legal historians (Landes, Deazley, Sherman) corroborate that the statute was both a response to publishers' economic pressure and a reframing of monopoly as property incentive. Scholars of the printing trade (McKitterick, Plant) document the rising pressure from unlicensed competition and the Stationers' Company's declining ability to enforce its monopoly informally.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__entangled_event_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__entangled_event_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__entangled_event_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__entangled_event_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__entangled_event_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__entangled_event_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statute_of_anne_ip_foundation__entangled_event_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statute_of_anne_ip_foundation__entangled_event_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading treats the statute as the singular event that MAKES copyright thinkable and institutionally real at the same moment. Extractiveness (0.62) is moderate-high because the statute converts informal monopoly into explicit legal extraction from readers and scholars, yet the framing as property incentive legitimates this extraction. Suppression (0.41) is moderate because the statute creates clarity and legal certainty (reducing the need for raw force) while establishing legal standing to enforce against piracy. Theater (0.28) is modest because the statute's preamble genuinely articulates an incentive-to-learning justification, but a growing share of enforcement activity in the post-1710 period defends monopoly against pirated copies rather than merely incentivizing new authorship. The measurement series show extractiveness rising slightly to 1755 as enforcement capacity matures, then stabilizing—the statute's institutional architecture is in place by mid-century, suppression_requirement rises modestly as litigation over the statute's boundaries becomes common (Case of Millar v. Taylor, 1769), and theater_ratio remains low because the incentive function continues to be the statute's legitimating narrative even as publishers consolidate power.
 *
 * PERSPECTIVAL GAP:
 *   From the publisher perspective: genuine coordination enabling market transactions + beneficial rent extraction from the legal monopoly. From the reader perspective: pure extraction converting informal monopoly into explicit legal restriction with no corresponding benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Book publishers are nominal beneficiaries (they are named in the statute's preamble as collectors of the right) but become practical extractors through publisher-drafted assignment contracts that bind authors to transfer the right for flat fees. Nascent author professionals are nominal beneficiaries (they are named as the rightholders) but are powerless and immediately instrumentalized—their exit option is 'constrained' because without the statute they have no legal standing at all, yet the statute's legal standing is immediately colonized by publisher contracts. Readers bear the cost of restriction without collecting any direct benefit—they are victims because the statute converts their prior (contestable) commons access into explicit legal prohibition. The conceptual clarity that would attach copyright unambiguously to authors is the victim because the entanglement of concept and institution means no clean frame can ever separate authorship from monopoly. Directionality for publishers is d≈0.15 (primary beneficiary, but moderate exit—they could rely on informal monopoly or informal trade practices if the statute vanished; mobile relative to a powerless author). Directionality for authors is d≈0.7 (trapped by legal standing that is immediately captured by publisher contracts; identity_locked because the category 'author' is created by the statute and cannot be inhabited outside it). Directionality for readers is d≈0.85 (full target; trapped by the statute's enforcement; no alternative access).  Directionality for conceptual_clarity is d≈1.0 (full victim; the entanglement is what destroys the ability to articulate copyright as a pure concept of author's rights rather than monopoly law).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's tangled_rope claim hinges on the inseparability of coordination and extraction. If the statute is read as 'pure extraction' (institutional_reallocation_reading) or 'pure concept-creation' (conceptual_emergence_reading), it becomes either a snare or a rope and the entanglement dissolves. The mandatrophy question is: does the statute persist because it solves a genuine coordination problem (statutory monopoly is more efficient than informal monopoly), or does it persist because it captures the normative authority to name authorship in the first place (whoever controls the legal frame controls what 'author' means)? This reading denies the dichotomy—the statute persists precisely because it does both, simultaneously, and the moment you try to separate them you lose the power of the statute to legitimate monopoly as property incentive. The constraint is not mandatrophic (the founding problem—how to stabilize monopoly while appearing to incentivize learning—is still live 60 years later), but the reading claims this very non-mandatrophy depends on the entanglement. If the entanglement becomes clear (if readers, scholars, and legal theorists successfully articulate the statute as 'monopoly law disguised as property'), the constraint's legitimacy could collapse because the mandated function (incentive to learning) and the actual function (rent extraction) would be visible as distinct, making the statute look like a snare and inviting reform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    concept_first_or_institution_first,
    'Did the concept of ''copyright as authored property'' emerge conceptually before the statute institutionalized it, or did the statute''s institutional act create the concept retroactively?',
    'Analysis of pre-1710 legal manuscripts, parliamentary debates, and writings by stationers and legal theorists to trace when ''copyright'' as a concept first appears in English discourse, relative to the statute''s codification.',
    'If the concept precedes the statute, the reading collapses into institutional_reallocation_reading (the statute merely institutionalized an already-thinkable idea). If the statute creates the concept, the entanglement claim is sustained (no meaningful separation between concept and institution). If both emerge together with no temporal precedence, the entanglement is the correct frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(concept_first_or_institution_first, empirical, 'Whether copyright as a concept was articulated before the statute institutionalized it.').

omega_variable(
    entanglement_vs_mutual_constitution,
    'Is the entanglement of concept and institution a feature of this constraint''s actual historical structure, or a feature of how entangled_event_reading CHOOSES to narrate it?',
    'Comparative analysis of how other legal innovations (patent law, trademark law) emerged: did institutional and conceptual change occur simultaneously in those cases, or can they be cleanly separated? If separability is the norm, entanglement is a reading choice; if simultaneity is the pattern, entanglement is structural.',
    'If entanglement is a reading choice, the constraint''s classification is frame-dependent (coexists_with the other readings). If entanglement is structural, this reading captures something the other readings miss, and the engine should flag those other readings as incomplete even if they compute as coherent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(entanglement_vs_mutual_constitution, conceptual, 'Whether the constraint''s entanglement is a discovered historical fact or a reading''s interpretive frame.').

omega_variable(
    author_agency_vs_publisher_capture,
    'To what extent did the statute''s nominal empowerment of authors as rightholders represent genuine authorship agency, as opposed to a legally-clarified form through which publishers could more efficiently capture authorial output?',
    'Examination of publisher-author contracts from 1710-1770 to assess what share of authors retained manuscript rights, renewal rights, or negotiating leverage; comparison to pre-statutory practices where working authors could negotiate directly with stationers or printers.',
    'If authors genuinely gained agency (many retained rights, negotiated favorable terms, built independent publishing ventures), the constraint is a tangled rope with real coordination benefit to the nascent author profession. If publishers immediately captured the right through standard-form contracts, the statute is more snare than rope, with nominal authorship as cover for publisher monopoly extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(author_agency_vs_publisher_capture, empirical, 'Whether author empowerment was real or immediately colonized by publisher capture.').

omega_variable(
    reading_boundary_foreclosure,
    'Does the entangled_event_reading''s core claim (that concept and institution cannot be disentangled without falsifying history) foreclose the institutional_reallocation_reading''s core claim (that the statute reallocates rights between institutions, a purely institutional fact)?',
    'Framing analysis: if one can coherently articulate the statute as a reallocation of rights without making claims about conceptual emergence or innovation, then the readings coexist; if any such articulation requires either denying that the statute created the concept of copyright or accepting that concept and institution are separable, then the readings are in tension.',
    'If foreclosure holds, only one reading can be true; the engine should assign a strict contradiction to the reading_relations. If coexistence holds, different frames can each be valid from their own perspective. If influence holds, entangled_event_reading undercuts institutional_reallocation_reading by denying its presumption of separability without logically foreclosing the institutional facts it describes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_foreclosure, conceptual, 'Whether the entanglement claim logically forecloses institutional separation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__entangled_event_reading, 1710, 1770).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1710, 0.25).
narrative_ontology:measurement_basis(stat_tr_t1710, observed).
narrative_ontology:measurement(stat_tr_t1725, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1725, 0.26).
narrative_ontology:measurement_basis(stat_tr_t1725, observed).
narrative_ontology:measurement(stat_tr_t1740, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1740, 0.28).
narrative_ontology:measurement_basis(stat_tr_t1740, observed).
narrative_ontology:measurement(stat_tr_t1755, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1755, 0.29).
narrative_ontology:measurement_basis(stat_tr_t1755, observed).
narrative_ontology:measurement(stat_tr_t1770, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1770, 0.28).
narrative_ontology:measurement_basis(stat_tr_t1770, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1710, 0.55).
narrative_ontology:measurement_basis(stat_be_t1710, observed).
narrative_ontology:measurement(stat_be_t1725, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1725, 0.59).
narrative_ontology:measurement_basis(stat_be_t1725, observed).
narrative_ontology:measurement(stat_be_t1740, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1740, 0.62).
narrative_ontology:measurement_basis(stat_be_t1740, observed).
narrative_ontology:measurement(stat_be_t1755, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1755, 0.63).
narrative_ontology:measurement_basis(stat_be_t1755, observed).
narrative_ontology:measurement(stat_be_t1770, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1770, 0.62).
narrative_ontology:measurement_basis(stat_be_t1770, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1710, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1710, 0.38).
narrative_ontology:measurement_basis(stat_su_t1710, observed).
narrative_ontology:measurement(stat_su_t1725, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1725, 0.4).
narrative_ontology:measurement_basis(stat_su_t1725, observed).
narrative_ontology:measurement(stat_su_t1740, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1740, 0.41).
narrative_ontology:measurement_basis(stat_su_t1740, observed).
narrative_ontology:measurement(stat_su_t1755, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1755, 0.42).
narrative_ontology:measurement_basis(stat_su_t1755, observed).
narrative_ontology:measurement(stat_su_t1770, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1770, 0.41).
narrative_ontology:measurement_basis(stat_su_t1770, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__entangled_event_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statute_of_anne_ip_foundation__entangled_event_reading, 0.18).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation__institutional_reallocation_reading).

% DUAL FORMULATION NOTE:
% statute_of_anne_ip_foundation is a contested kernel with three structurally distinct constraint readings: entangled_event_reading (this story) holds that conceptual and institutional change are simultaneous and undecomposable; conceptual_emergence_reading separates the emergence of copyright as a new concept from institutional reallocation; institutional_reallocation_reading separates the reallocation of monopoly rights from conceptual change. All three share the same legal text (the statute) and the same historical event, but disagree on what kind of thing the event IS. The three stories are linked via network.affects_constraints and cs_structure.reading_relations to document the contest. The ε value (0.62) in this reading reflects the entanglement claim: the constraint extracts by converting informal monopoly into explicit legal restriction, yet legitimates this extraction by framing it as author incentive—the coordination function and the extraction function are inseparable. The sibling readings attribute different ε values to different factual claims: conceptual_emergence_reading might author lower ε (if the concept's creation is genuinely valuable), institutional_reallocation_reading might author higher ε (if the reallocation is pure extraction). All three are valid constraint stories from their respective frames.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
