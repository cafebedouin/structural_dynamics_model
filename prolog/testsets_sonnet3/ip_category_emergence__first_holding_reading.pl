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
 *   human_readable: Statute of Anne (1710) as Occupancy Shift — Author Enters the Legitimate Claimant Set
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This story reads the Statute of Anne (1710) narrowly as an occupancy
 *   event: a change in the MEMBERSHIP of the set of parties who may
 *   legitimately hold an original claim over a printed work's reproduction.
 *   Before 1710 that set was effectively the Stationers' Company's internal
 *   register, controlled by senior members; after 1710 the statute names the
 *   author as the point of original statutory grant, even though authors
 *   typically immediately reassign the right to a bookseller. This reading
 *   does not claim the Act made 'ownable expression' newly THINKABLE as a
 *   category (that is the sibling thinkability_reading, a distinct constraint
 *   with its own epsilon and stakeholder set) — it claims something narrower
 *   and more historically concrete: an existing kind of claim (exclusive
 *   reprint control) changed WHO could originate it, shifting the enforcement
 *   beneficiary from an internal trade-guild register to a
 *   statutorily-created, judicially enforceable individual grant. The
 *   subsequent contest between Stationers' incumbents (defending a perpetual
 *   common-law claim) and reform booksellers (wanting the term-limited
 *   statutory claim to preempt the perpetual one), resolved only in Donaldson
 *   v Becket (1774), is read here as the direct continuation of the occupancy
 *   dispute this constraint names.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, 0.58).
domain_priors:suppression_score(ip_category_emergence__first_holding_reading, 0.52).
domain_priors:theater_ratio(ip_category_emergence__first_holding_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__first_holding_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__first_holding_reading, "Statute of Anne (1710) as Occupancy Shift — Author Enters the Legitimate Claimant Set").
narrative_ontology:topic_domain(ip_category_emergence__first_holding_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__first_holding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__first_holding_reading, 'e1052a57-02d5-4b65-a3c1-0fda915ee91d').
narrative_ontology:cs_kernel_codification('e1052a57-02d5-4b65-a3c1-0fda915ee91d', fixed_text).
narrative_ontology:cs_authority_grounding('e1052a57-02d5-4b65-a3c1-0fda915ee91d', lineage).
narrative_ontology:cs_interpretation_layer_present('e1052a57-02d5-4b65-a3c1-0fda915ee91d').
narrative_ontology:cs_reading_relation('e1052a57-02d5-4b65-a3c1-0fda915ee91d', ip_category_emergence__thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('e1052a57-02d5-4b65-a3c1-0fda915ee91d', ip_category_emergence__synchronic_diachronic_seam, influences).
narrative_ontology:cs_axiom('e1052a57-02d5-4b65-a3c1-0fda915ee91d', foundational, claimant_set_membership_is_the_operative_change).
narrative_ontology:cs_axiom_status(claimant_set_membership_is_the_operative_change, holdable).
narrative_ontology:cs_axiom_grounding('e1052a57-02d5-4b65-a3c1-0fda915ee91d', claimant_set_membership_is_the_operative_change, empirically_contingent).
narrative_ontology:cs_axiom('e1052a57-02d5-4b65-a3c1-0fda915ee91d', secondary, statutory_grant_supersedes_perpetual_registry_claim).
narrative_ontology:cs_axiom_status(statutory_grant_supersedes_perpetual_registry_claim, holdable).
narrative_ontology:cs_axiom_grounding('e1052a57-02d5-4b65-a3c1-0fda915ee91d', statutory_grant_supersedes_perpetual_registry_claim, conventional).
narrative_ontology:cs_reference_frame('e1052a57-02d5-4b65-a3c1-0fda915ee91d', stationers_perpetual_register_control).
narrative_ontology:cs_drift_state('e1052a57-02d5-4b65-a3c1-0fda915ee91d', post_donaldson_1774, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('e1052a57-02d5-4b65-a3c1-0fda915ee91d', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__first_holding_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, statutory_authors).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, reform_minded_booksellers).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, stationers_company_incumbents).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, provincial_reprint_trade).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, provincial_reprint_trade).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, reading_public).
narrative_ontology:constraint_vindicates(ip_category_emergence__first_holding_reading, authorial_labor_desert_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Before 1710, authors typically transferred manuscripts outright to stationers and had no continuing legal claim once a copy was sold or licensed. The 1710 Act names the author as an original locus of statutory right (fourteen years, renewable once if living), which is the seat this reading tracks: the occupied set of 'who may legitimately hold a claim over a text's copying' now includes the author as such, not merely as a prior owner who has already alienated the interest. Most authors immediately assign the right to a bookseller for payment, so the practical benefit is thin, but the categorical membership is new.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, statutory_authors, beneficiary,
    moderate, biographical, constrained, national).

% A faction of London booksellers outside the entrenched Stationers' Company leadership, and provincial/Scottish printers, pushed Parliament for a statutory scheme that would break the perpetual common-law claim the Company asserted through its own register. They benefit from a fixed statutory term because it opens the reprint market once the term lapses, and from the author-as-rightsholder frame because it lets them contract directly with authors rather than through the Company's internal registry monopoly.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, reform_minded_booksellers, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__first_holding_reading, reform_minded_booksellers, agenda_setter).

% The Company's core assets were perpetual common-law copies registered internally, controlled by senior members regardless of any statute. The 1710 Act's fixed term and its routing of the initial right through the author (who then assigns it) undercuts the Company's self-administered perpetual-claim regime. Incumbents cannot simply exit the print trade; their capital is sunk in existing copy-holdings whose legal footing the Act destabilizes, and they spend the following decades litigating for a perpetual common-law copyright to route around the statute (culminating in Donaldson v Becket, 1774).
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, stationers_company_incumbents, payer,
    powerful, generational, trapped, national).

% Scottish and provincial English printers who had operated in a legal gray zone reprinting London-registered works. The statutory term, once understood to be finite and to preempt perpetual common-law claims, eventually opens reprint opportunity for them — but during the period this reading tracks (the immediate post-1710 decades) they are also newly exposed to a national, court-enforceable term-based claim where before enforcement was largely a Company-internal registry matter with uncertain extraterritorial reach. Their position shifts twice within the interval, which the temporal measurements below track as a genuine drift rather than a static value.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, provincial_reprint_trade, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__first_holding_reading, provincial_reprint_trade, beneficiary).

% Buyers and borrowers of books. A finite statutory term, if honored, promises eventual public-domain entry that a perpetual Company copy-claim would never have delivered. They have no seat in the drafting of the Act and no direct enforcement power; their interest is represented, if at all, by the reform booksellers' commercial argument for open reprinting after term expiry, not by any voice of their own.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, reading_public, beneficiary,
    powerless, generational, trapped, national).

% Adjudicate, across the ensuing decades (Millar v Taylor 1769, Donaldson v Becket 1774), whether the statute displaced or merely supplemented an underlying perpetual common-law authorial or stationer's right. Their rulings retroactively determine what the 1710 occupancy shift actually accomplished — whether the author's entry into the claimant set was a genuine new membership or a procedural gloss on a claim that always could have been read as author-originating.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, kings_bench_and_common_law_judges, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a single national, judicially enforceable term for the right to copy a printed work, replacing a patchwork of Company-internal perpetual registry claims with a public, dated, renewable statutory grant — solving the coordination problem of who may sue whom over reprinting, on what evidence, for how long.
% TRANSFER_FUNCTION: Moves the formal locus of the original claim from the Stationers' Company's internal register (where incumbent members controlled entry) to the author, who typically immediately reassigns it to a bookseller for payment; net commercial value still flows overwhelmingly to booksellers, but the legal starting point of the chain has moved.
% ABSENT_VOICES: The reading public who would eventually benefit from term expiry have no drafting voice at all. Individual authors as a class had limited direct influence on the 1710 text, which was substantially shaped by the commercial dispute between rival bookseller factions; the author-protection language was in part a rhetorical vehicle for the reform booksellers' commercial aim of breaking the Company's perpetual claim.
% DISAPPEARANCE_RATIONALE: If the 1710 occupancy shift were undone, the legitimate claimant set reverts to whoever the Stationers' Company's internal register recognized (senior Company members holding registered copies), with no independent author-originating claim and no statutorily fixed term — the entire subsequent case-law struggle over perpetual versus term-limited copyright, and the eventual public-domain concept, would have no textual anchor to develop from.
% FOUNDING_PROBLEM: The Stationers' Company's licensing monopoly, which had provided crown-enforced censorship control and internal trade discipline, lapsed with the Licensing Act's expiry in 1695, leaving the book trade without a stable, court-enforceable mechanism to prevent unauthorized reprinting; rival bookseller factions and the Company's leadership both wanted Parliament to supply one, but disagreed sharply about its form and duration.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside any bookseller or publisher interest (e.g., the modern historiography surrounding Donaldson v Becket and the pre-1710 licensing lapse) attest that the specific 1695 vacuum the Act was built to fill was resolved by the Act's passage itself and by the subsequent judicial settlement in 1774; the copyright term mechanism has persisted for centuries past the resolution of that original coordination gap, now serving primarily as an inherited allocation framework rather than a live response to an unregulated reprinting crisis.
narrative_ontology:disappearance_verdict(ip_category_emergence__first_holding_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__first_holding_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__first_holding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises steadily from 1695 (0.42, pure Company-internal control with no statutory overlay) through the pre-Donaldson peak around 1769 (0.63, when Millar v Taylor affirmed a perpetual common-law copyright ALONGSIDE the statute, doubly entrenching claimant control) and then eases slightly by 1774 (0.58) once Donaldson v Becket foreclosed the perpetual common-law claim and left only the statutory term. Suppression tracks the same arc: the fixed term required active litigation and judicial enforcement to make it bind against a Company that kept asserting a perpetual claim through its own registry practice, peaking as that fight intensified through the 1760s. Theater ratio rises modestly across the interval as booksellers on both sides increasingly used author-protection rhetoric instrumentally — invoking 'the author's natural right' in court arguments whose real stakes were which trade faction would control reprint revenue, a performative layer over the underlying commercial dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   Reform booksellers and statutory authors are coded as beneficiaries: the former gain a legal lever to break the Company's perpetual internal claim and contract directly around it; the latter gain formal (if largely nominal, given near-universal assignment) membership in the claimant set. Stationers' Company incumbents are the clearest payers — their entire prior business model rested on a perpetual, internally-administered claim that the statute displaced and that took 64 years of litigation to fully dislodge. Provincial reprint trade is coded dual-role because their position inverts across the interval: initially newly exposed to nationally enforceable claims they previously evaded, later positioned to benefit once the term-limited (not perpetual) reading prevailed in 1774 — the temporal measurements are authored to reflect this genuine reversal rather than picking one snapshot.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (the 1695 lapse of licensing-based trade control) is dead by any external legal-historical account — it was resolved definitively by 1774. But the occupancy structure the 1710 Act created (author as statutory point-of-origin, immediately assignable) persists for centuries as the base architecture of copyright law, now serving new purposes (media-industry rights administration, corporate authorship doctrines) unconnected to the original stationers'-guild dispute. Classifying this as tangled_rope rather than a resolved scaffold captures that the coordination function (a workable, dateable, enforceable claim structure) is real and still operates, while the specific extraction dynamic (which faction captures assignment value) has simply changed hands repeatedly rather than disappearing — it is not a temporary transitional device with a sunset, it is a durable structure that has outlived the crisis that prompted it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    occupancy_vs_thinkability_independence,
    'Is the 1710 occupancy shift (author enters the claimant set) genuinely independent of the thinkability shift (ownable expression becomes a coherent legal category), or are they the same underlying event described at two different levels of abstraction — one synchronic (a category exists or does not), one diachronic (a membership changes at a moment)?',
    'This is precisely the question the sibling constraint synchronic_diachronic_seam exists to test (M4/M5 collapse test in the source manifest): if the two framings always co-vary across every historical instance examined (no case where occupancy changes without thinkability changing, or vice versa), they collapse into one constraint; if a dissociating case is found (e.g., a jurisdiction where authorial claimant status was recognized without a corresponding shift in what counted as ownable expression, or the reverse), they remain formally distinct.',
    'If the readings collapse, this constraint''s separate epsilon and stakeholder authoring becomes redundant with thinkability_reading and the two should be merged into a single constraint file; if they remain distinct, the network edge to thinkability_reading documents genuine but separate structural claims about the same historical moment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(occupancy_vs_thinkability_independence, conceptual, 'Whether first-holding and thinkability are the same event under two descriptions or genuinely separable structural claims.').

omega_variable(
    authorial_membership_substance_vs_form,
    'Did the author''s statutory entry into the claimant set constitute a substantive change in who held economic control, given that authors almost universally reassigned the right to booksellers immediately upon receiving it — or was the author''s naming in the statute primarily a rhetorical and legal-technical device serving the reform booksellers'' actual goal of breaking the Stationers'' Company''s perpetual claim?',
    'Archival analysis of surviving 1710s-1720s publishing contracts and assignment deeds: if authors near-universally assigned the right within days of any registration, and assignment terms show little author bargaining leverage, the ''author benefit'' reading is substantially formal rather than substantive.',
    'If substantially formal, the beneficiary coding of statutory_authors should be weakened or treated as instrumental rather than a genuine capture of the coordination benefit — most of the real gain would flow to reform_minded_booksellers, sharpening the tangled_rope reading toward a bookseller-faction contest wearing author-rights language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorial_membership_substance_vs_form, empirical, 'Whether authorial claimant status in 1710 was economically substantive or primarily a legal-rhetorical vehicle for a bookseller-faction dispute.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__first_holding_reading, 1695, 1774).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1695, ip_category_emergence__first_holding_reading, theater_ratio, 1695, 0.15).
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__first_holding_reading, theater_ratio, 1710, 0.22).
narrative_ontology:measurement(ip_c_tr_t1725, ip_category_emergence__first_holding_reading, theater_ratio, 1725, 0.26).
narrative_ontology:measurement(ip_c_tr_t1740, ip_category_emergence__first_holding_reading, theater_ratio, 1740, 0.28).
narrative_ontology:measurement(ip_c_tr_t1755, ip_category_emergence__first_holding_reading, theater_ratio, 1755, 0.3).
narrative_ontology:measurement(ip_c_tr_t1769, ip_category_emergence__first_holding_reading, theater_ratio, 1769, 0.34).
narrative_ontology:measurement(ip_c_tr_t1774, ip_category_emergence__first_holding_reading, theater_ratio, 1774, 0.3).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1695, ip_category_emergence__first_holding_reading, base_extractiveness, 1695, 0.42).
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__first_holding_reading, base_extractiveness, 1710, 0.5).
narrative_ontology:measurement(ip_c_be_t1725, ip_category_emergence__first_holding_reading, base_extractiveness, 1725, 0.55).
narrative_ontology:measurement(ip_c_be_t1740, ip_category_emergence__first_holding_reading, base_extractiveness, 1740, 0.58).
narrative_ontology:measurement(ip_c_be_t1755, ip_category_emergence__first_holding_reading, base_extractiveness, 1755, 0.6).
narrative_ontology:measurement(ip_c_be_t1769, ip_category_emergence__first_holding_reading, base_extractiveness, 1769, 0.63).
narrative_ontology:measurement(ip_c_be_t1774, ip_category_emergence__first_holding_reading, base_extractiveness, 1774, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1695, ip_category_emergence__first_holding_reading, suppression_requirement, 1695, 0.35).
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__first_holding_reading, suppression_requirement, 1710, 0.48).
narrative_ontology:measurement(ip_c_su_t1725, ip_category_emergence__first_holding_reading, suppression_requirement, 1725, 0.52).
narrative_ontology:measurement(ip_c_su_t1740, ip_category_emergence__first_holding_reading, suppression_requirement, 1740, 0.56).
narrative_ontology:measurement(ip_c_su_t1755, ip_category_emergence__first_holding_reading, suppression_requirement, 1755, 0.6).
narrative_ontology:measurement(ip_c_su_t1769, ip_category_emergence__first_holding_reading, suppression_requirement, 1769, 0.65).
narrative_ontology:measurement(ip_c_su_t1774, ip_category_emergence__first_holding_reading, suppression_requirement, 1774, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the ip_category_emergence kernel. first_holding_reading (this file) claims a membership/occupancy shift in the legitimate claimant set. thinkability_reading claims a category-emergence shift (ownable expression becoming legally coherent). synchronic_diachronic_seam tests whether these two claims are formally independent or a temporal-framing artifact of one underlying shift. Each carries its own epsilon, stakeholder set, and claimed type per the ε-invariance principle; they are linked here rather than merged because their beneficiary/victim structures and empirical commitments differ (this reading's central dispute is Stationers' incumbents vs. reform booksellers over enforcement locus; thinkability_reading's central dispute would be over conceptual availability of the ownable-expression category itself, a different question with potentially different victims).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
