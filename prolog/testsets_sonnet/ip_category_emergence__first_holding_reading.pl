% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__first_holding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__first_holding_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: ip_category_emergence__first_holding_reading
 *   human_readable: Statute of Anne Occupancy Shift — Author as Legitimate Rights-Claimant (1710)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This story treats the 1710 Statute of Anne strictly as a MEMBERSHIP-SHIFT
 *   event: who counts among the legitimate claimants to a reproduction right.
 *   Before the Act, the occupied set was 'Stationers' Company registrants';
 *   after the Act, the occupied set is 'authors (and their statutory
 *   assignees).' This is deliberately distinct from the question of whether
 *   ownable expression itself became a newly THINKABLE legal category in 1710
 *   (that is a separate constraint, thinkability_reading) and from the
 *   question of whether the two framings collapse into one under formal
 *   analysis (synchronic_diachronic_seam). Per the ε-invariance principle,
 *   these are three different constraints with three different extraction
 *   profiles and are not merged here.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, 0.58).
domain_priors:suppression_score(ip_category_emergence__first_holding_reading, 0.62).
domain_priors:theater_ratio(ip_category_emergence__first_holding_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__first_holding_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__first_holding_reading, "Statute of Anne Occupancy Shift — Author as Legitimate Rights-Claimant (1710)").
narrative_ontology:topic_domain(ip_category_emergence__first_holding_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__first_holding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__first_holding_reading, '2f8397aa-458f-4f65-b1e8-5c34f61dac6d').
narrative_ontology:cs_kernel_codification('2f8397aa-458f-4f65-b1e8-5c34f61dac6d', formalized).
narrative_ontology:cs_authority_grounding('2f8397aa-458f-4f65-b1e8-5c34f61dac6d', lineage).
narrative_ontology:cs_interpretation_layer_present('2f8397aa-458f-4f65-b1e8-5c34f61dac6d').
narrative_ontology:cs_reading_relation('2f8397aa-458f-4f65-b1e8-5c34f61dac6d', ip_category_emergence__thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f8397aa-458f-4f65-b1e8-5c34f61dac6d', ip_category_emergence__synchronic_diachronic_seam, influences).
narrative_ontology:cs_axiom('2f8397aa-458f-4f65-b1e8-5c34f61dac6d', foundational, author_is_origin_point_of_claim).
narrative_ontology:cs_axiom_status(author_is_origin_point_of_claim, holdable).
narrative_ontology:cs_axiom_grounding('2f8397aa-458f-4f65-b1e8-5c34f61dac6d', author_is_origin_point_of_claim, conventional).
narrative_ontology:cs_axiom('2f8397aa-458f-4f65-b1e8-5c34f61dac6d', secondary, guild_membership_no_longer_gates_legitimacy).
narrative_ontology:cs_axiom_status(guild_membership_no_longer_gates_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('2f8397aa-458f-4f65-b1e8-5c34f61dac6d', guild_membership_no_longer_gates_legitimacy, conventional).
narrative_ontology:cs_reference_frame('2f8397aa-458f-4f65-b1e8-5c34f61dac6d', guild_registration_monopoly).
narrative_ontology:cs_drift_state('2f8397aa-458f-4f65-b1e8-5c34f61dac6d', post_millar_v_taylor_1769, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2f8397aa-458f-4f65-b1e8-5c34f61dac6d', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__first_holding_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, individual_authors).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, emerging_print_capitalists).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, stationers_company_incumbents).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, unlicensed_provincial_printers).
narrative_ontology:constraint_vindicates(ip_category_emergence__first_holding_reading, authorial_labor_desert_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Before 1710, authors held no statutory claim once a manuscript was sold or entered in the Stationers' Register — the perpetual copy belonged to the guild member who registered it. The 1710 Act names the author as the first legitimate holder of a term-limited right, for the first time putting a living, non-guild individual inside the set of parties whose claim the state will enforce. Authors gain a marketable entitlement but must still contract it away to booksellers to see any income, so the shift changes who is COUNTED as a rights-holder more than it changes who collects.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, individual_authors, beneficiary,
    moderate, biographical, constrained, national).

% Booksellers and publishers outside the old Stationers' oligopoly lobby for the Act because it lets them acquire rights directly from authors rather than depending on Stationers' Register entries controlled by incumbent guild members. They become the practical enforcers and beneficiaries of the new occupancy pattern even though the statute is framed around the author.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, emerging_print_capitalists, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__first_holding_reading, emerging_print_capitalists, agenda_setter).

% Hold perpetual common-law copy under guild custom and lobbied Parliament themselves for the 1710 Act, expecting it to formalize their existing perpetual monopoly. Instead the Act's term limits and author-first structure erode their exclusive claimant status — their registry loses its monopoly function as the sole gate of legitimate ownership. They are structurally displaced from being the exclusive occupants of the rights-holder set.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, stationers_company_incumbents, payer,
    organized, generational, constrained, national).

% Small printers outside London who previously operated in gray-market defiance of the Stationers' monopoly now face a differently-shaped but still exclusionary enforcement regime: statutory copyright backed by court action rather than guild custom. The occupancy set changed membership (author replaces guild as origin-point) but the exclusion of unauthorized reproduction persists against them in a new legal form.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, unlicensed_provincial_printers, payer,
    powerless, biographical, trapped, regional).

% Drafts and enacts the statute, replacing royal-charter-backed guild monopoly (a censorship-adjacent licensing tool) with a court-enforceable, term-limited property right. Parliament's interest is partly in curbing both perpetual guild monopoly and unregulated printing, using the author-first framing as the legitimating vehicle for a new enforcement architecture.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, crown_and_parliament, agenda_setter,
    institutional, civilizational, analytical, national).

% Examine the 1710 Act's text and enforcement record to determine whether it enacts a change in WHO occupies the legitimate-claimant set (this reading) as distinct from a change in WHAT KIND of thing (ownable expression) becomes legally thinkable — the two questions are analytically separable and are treated as separate constraints in this framework.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, publicly registrable, court-enforceable procedure for determining who holds a reproduction right in a printed work, replacing a patchwork of guild custom, royal charter, and ad hoc licensing that previously required Stationers' Company membership to access at all.
% TRANSFER_FUNCTION: Moves the formal position of 'origin-point of a legitimate reproduction claim' from the Stationers' Company's registered guild member to the author (and their assignees), while the practical economic benefit of that repositioned claim still flows largely to the booksellers who purchase the right from the author.
% ABSENT_VOICES: Provincial and unlicensed printers, whose interests in unrestricted reproduction were never part of the Parliamentary negotiation between the Stationers' Company and the reformist booksellers; their exclusion from the legitimate-claimant set persists across the reform, just administered under new statutory rather than guild-custom machinery.
% DISAPPEARANCE_RATIONALE: If the 1710 occupancy shift were undone, the legitimate-claimant set would revert to guild-registered stationers only; authors would have no independent statutory standing to sell or license their own works, and the entire subsequent architecture of authorial copyright — reversion rights, term limits keyed to the author's act of creation — would lack its founding membership premise.
% FOUNDING_PROBLEM: Perpetual guild monopoly under the Stationers' Company had become both a printing-trade restraint (locking out new booksellers and provincial printers) and a de facto censorship mechanism inherited from Crown licensing; Parliament sought a claimant structure that could be time-limited and did not require guild membership as the gate to legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary Parliamentary debate records and later legal historians (e.g. commentary independent of both the Stationers' Company and the publishing trade) attest the founding problem was real — guild monopoly and licensing-era censorship were genuine grievances — but debate persists among historians whether the author-first framing was a substantive membership change or a legitimating fiction serving the same booksellers who previously worked through the Stationers' registry.
narrative_ontology:disappearance_verdict(ip_category_emergence__first_holding_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__first_holding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__first_holding_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ip_category_emergence__first_holding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__first_holding_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__first_holding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ip_category_emergence__first_holding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness declines sharply at 1710 (0.68 to 0.55) reflecting the real coordination gain of a public, term-limited, non-monopolistic registration system replacing perpetual guild exclusivity — genuine reduction in extraction concentrated in one incumbent guild. It creeps back upward after 1710 as booksellers develop assignment practices (perpetual assignment contracts, 'literary property' common-law claims through Millar v. Taylor in 1769) that partially reconstitute bookseller-side extraction using the author's new statutory standing as the vehicle. Suppression drops at the formal moment of reform (guild monopoly enforcement relaxes) but the underlying exclusionary function against unlicensed printers persists at a lower but non-trivial level throughout, now backed by statute and courts rather than guild custom.
 *
 * PERSPECTIVAL GAP:
 *   From the Stationers' Company's historical seat, the 1710 Act reads as expropriation of settled property (their perpetual copy). From the author's seat, it reads as a genuine, if incomplete, elevation to legal personhood as a rights-originator. From the provincial printer's seat, nothing structural changes — enforcement against unauthorized reproduction merely changes its citation. The engine should register these as different seat-level classifications from the same structural facts, not as competing 'interpretations' requiring reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Authors are named beneficiaries of the occupancy shift (they enter the legitimate-claimant set) but their d is only moderately toward the beneficiary end because most authors immediately assign rights to publishers for want of capital to exploit them directly — the FORMAL membership change does not track the PRACTICAL extraction pattern, which is why print capitalists are also coded beneficiary/agenda_setter. Stationers' incumbents are the clearest payer: they lose their exclusive occupancy of the claimant set, a direct structural cost even though many individual Stationers also became copyright holders as publishers. Provincial printers remain trapped victims across the reform — their exclusion from legitimate reproduction is the one continuity the occupancy shift does NOT alter.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (curbing guild monopoly and licensing-era censorship) is genuinely partially resolved by the occupancy shift, but the enforcement machinery that grew from it (perpetual assignment contracts, later statutory extensions) increasingly serves publisher extraction rather than the author-centered legitimating story used to pass the original Act — a classic case where declaring the mandate simply 'resolved' or simply 'unresolved' would misclassify the structure; it requires the tangled_rope reading (real coordination function + persistent asymmetric extraction) rather than either a clean rope or a pure snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    membership_vs_category_independence,
    'Is the 1710 shift in WHO holds legitimate claims (this reading) analytically separable from a shift in WHAT KIND of thing can be owned (thinkability_reading), or do these collapse into a single event under closer formal analysis?',
    'Formal analysis of whether the pre-1710 legal system could have accommodated author-claimants without any change in the ownability of expression itself (would support separability), versus whether author-standing presupposes the category shift (would support collapse) — this is precisely the question the synchronic_diachronic_seam sibling constraint is built to resolve.',
    'If the readings collapse, this constraint''s extraction profile should be merged with thinkability_reading''s; if they remain separable, the two profiles (and their differing extraction trajectories) are independently valid and this story''s claim/metric independence stands on its own.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(membership_vs_category_independence, conceptual, 'Whether first-holding and thinkability are one event or two under formal analysis.').

omega_variable(
    author_beneficiary_authenticity,
    'Was the 1710 author-first framing a genuine expansion of who could hold rights, or primarily a legitimating fiction constructed by reformist booksellers to break the Stationers'' monopoly, with authors serving as rhetorical rather than substantive beneficiaries?',
    'Examine the actual distribution of registered copyright assignments in the decades after 1710: if the overwhelming majority of authors assigned rights to booksellers within a short period of first registration, this supports the legitimating-fiction reading; if a meaningful minority of authors retained and exploited rights directly, this supports genuine membership expansion.',
    'If legitimating fiction, the beneficiary designation for individual_authors should be downgraded and the constraint reads closer to snare (booksellers using authors as a formal pass-through); if genuine, tangled_rope with authors as real (if minor) beneficiaries is the more accurate classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(author_beneficiary_authenticity, empirical, 'Whether authors were substantive or merely nominal beneficiaries of the occupancy shift.').

omega_variable(
    natural_vs_constructed_claimant_category,
    'Is ''author as origin-point of a property claim'' a naturalized extension of pre-existing labor-desert intuitions (Locke-style claims to the fruits of one''s labor), or a constructed legal category that only appears natural in retrospect because subsequent copyright doctrine builds on it?',
    'Comparative legal history: examine whether other print-culture jurisdictions without a Lockean philosophical tradition independently arrived at author-first claimant structures, which would support naturalization; if author-first structures appear only where Lockean-influenced legal reform occurred, this supports the constructed reading.',
    'Bears on whether the vindicated_propositions entry (authorial_labor_desert_doctrine) reflects a genuine natural-law-adjacent claim or a constructed doctrine that happens to serve the reformist coalition''s interests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_constructed_claimant_category, conceptual, 'Whether the author-as-origin-point category is naturalized or constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__first_holding_reading, 1662, 1774).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1662, ip_category_emergence__first_holding_reading, theater_ratio, 1662, 0.2).
narrative_ontology:measurement(ip_c_tr_t1700, ip_category_emergence__first_holding_reading, theater_ratio, 1700, 0.22).
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__first_holding_reading, theater_ratio, 1710, 0.25).
narrative_ontology:measurement(ip_c_tr_t1730, ip_category_emergence__first_holding_reading, theater_ratio, 1730, 0.28).
narrative_ontology:measurement(ip_c_tr_t1750, ip_category_emergence__first_holding_reading, theater_ratio, 1750, 0.29).
narrative_ontology:measurement(ip_c_tr_t1774, ip_category_emergence__first_holding_reading, theater_ratio, 1774, 0.3).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1662, ip_category_emergence__first_holding_reading, base_extractiveness, 1662, 0.68).
narrative_ontology:measurement(ip_c_be_t1700, ip_category_emergence__first_holding_reading, base_extractiveness, 1700, 0.63).
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__first_holding_reading, base_extractiveness, 1710, 0.55).
narrative_ontology:measurement(ip_c_be_t1730, ip_category_emergence__first_holding_reading, base_extractiveness, 1730, 0.5).
narrative_ontology:measurement(ip_c_be_t1750, ip_category_emergence__first_holding_reading, base_extractiveness, 1750, 0.54).
narrative_ontology:measurement(ip_c_be_t1774, ip_category_emergence__first_holding_reading, base_extractiveness, 1774, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1662, ip_category_emergence__first_holding_reading, suppression_requirement, 1662, 0.72).
narrative_ontology:measurement(ip_c_su_t1700, ip_category_emergence__first_holding_reading, suppression_requirement, 1700, 0.7).
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__first_holding_reading, suppression_requirement, 1710, 0.6).
narrative_ontology:measurement(ip_c_su_t1730, ip_category_emergence__first_holding_reading, suppression_requirement, 1730, 0.58).
narrative_ontology:measurement(ip_c_su_t1750, ip_category_emergence__first_holding_reading, suppression_requirement, 1750, 0.6).
narrative_ontology:measurement(ip_c_su_t1774, ip_category_emergence__first_holding_reading, suppression_requirement, 1774, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the ip_category_emergence kernel. first_holding_reading (this story) tracks membership change in the legitimate-claimant set. thinkability_reading tracks category emergence — whether ownable expression became a coherent legal object. synchronic_diachronic_seam tests whether these two are formally independent (M4/M5 collapse test) or artifacts of how the 1710 moment is temporally framed. Each carries its own ε and stakeholder structure per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
