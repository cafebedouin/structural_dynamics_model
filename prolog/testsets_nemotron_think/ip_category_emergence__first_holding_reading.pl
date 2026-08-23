% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__first_holding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Statute of Anne Author-Right Recognition (1710) — First Holding Reading
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   The Statute of Anne (1710) recognized authors as the original proprietors
 *   of copyright, displacing the Stationers' Company's perpetual common-law
 *   monopoly over printing. This first-holding reading treats the 1710 moment
 *   as a membership shift in the legitimate claimant set: the 'occupied seat'
 *   of rights-holder moved from Stationers to authors. The constraint is the
 *   legal framework that made author-as-rights-holder a coherent category and
 *   enforced the transfer of enforcement beneficiary. The Stationers'
 *   monopoly required active suppression of unauthorized presses; the new
 *   author-right required active enforcement against piracy but also against
 *   the Stationers' residual claims. The scaffold justification was the
 *   limited term (14+14 years) and the stated purpose of 'encouragement of
 *   learning' — a transitional coordination function meant to expire into
 *   public domain.
 *
 * KEY AGENTS:
 *   - authors_as_rights_holders: Primary beneficiary (moderate/biographical/constrained/national) — gained statutory recognition but depended on publishers for distribution
 *   - stationers_company: Primary victim (powerful/generational/trapped/national) — lost perpetual monopoly, forced into statutory framework
 *   - parliament_state: Agenda setter (institutional/civilizational/arbitrage/national) — enacted the statute, defined the category
 *   - publishers_booksellers: Secondary beneficiary/payer (organized/biographical/constrained/national) — intermediaries who contracted with authors
 *   - reading_public: Victim/beneficiary (organized/biographical/mobile/national) — faced higher prices initially, gained broader access long-term
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, 0.42).
domain_priors:suppression_score(ip_category_emergence__first_holding_reading, 0.58).
domain_priors:theater_ratio(ip_category_emergence__first_holding_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__first_holding_reading, scaffold).
narrative_ontology:human_readable(ip_category_emergence__first_holding_reading, "Statute of Anne Author-Right Recognition (1710) — First Holding Reading").
narrative_ontology:topic_domain(ip_category_emergence__first_holding_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__first_holding_reading).
narrative_ontology:has_sunset_clause(ip_category_emergence__first_holding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__first_holding_reading, '97e5456c-d830-449d-b57d-7906c3a44a37').
narrative_ontology:cs_kernel_codification('97e5456c-d830-449d-b57d-7906c3a44a37', formalized).
narrative_ontology:cs_authority_grounding('97e5456c-d830-449d-b57d-7906c3a44a37', lineage).
narrative_ontology:cs_interpretation_layer_present('97e5456c-d830-449d-b57d-7906c3a44a37').
narrative_ontology:cs_reading_relation('97e5456c-d830-449d-b57d-7906c3a44a37', ip_category_emergence__thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('97e5456c-d830-449d-b57d-7906c3a44a37', ip_category_emergence__synchronic_diachronic_seam, influences).
narrative_ontology:cs_axiom('97e5456c-d830-449d-b57d-7906c3a44a37', foundational, author_as_original_rights_holder).
narrative_ontology:cs_axiom_status(author_as_original_rights_holder, holdable).
narrative_ontology:cs_axiom_grounding('97e5456c-d830-449d-b57d-7906c3a44a37', author_as_original_rights_holder, conventional).
narrative_ontology:cs_axiom('97e5456c-d830-449d-b57d-7906c3a44a37', foundational, statutory_origin_of_author_right).
narrative_ontology:cs_axiom_status(statutory_origin_of_author_right, holdable).
narrative_ontology:cs_axiom_grounding('97e5456c-d830-449d-b57d-7906c3a44a37', statutory_origin_of_author_right, conventional).
narrative_ontology:cs_reference_frame('97e5456c-d830-449d-b57d-7906c3a44a37', stationers_perpetual_monopoly).
narrative_ontology:cs_drift_state('97e5456c-d830-449d-b57d-7906c3a44a37', post_donaldson_v_becket_1774, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('97e5456c-d830-449d-b57d-7906c3a44a37', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__first_holding_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, authors_as_rights_holders).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, parliament_state).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, stationers_company).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, reading_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, publishers_booksellers).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, reading_public).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, publishers_booksellers).
narrative_ontology:constraint_vindicates(ip_category_emergence__first_holding_reading, author_as_original_proprietor).
narrative_ontology:constraint_vindicates(ip_category_emergence__first_holding_reading, statutory_origin_of_copyright).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained statutory recognition as original proprietors of their works for the first time. Could assign or license copyright to publishers. But depended on publishers for distribution, marketing, and enforcement — few authors could self-publish at scale. Exit meant returning to patronage or abandoning writing as profession.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, authors_as_rights_holders, beneficiary,
    moderate, biographical, constrained, national).

% Held a perpetual common-law monopoly on printing via royal charter and internal registry. The 1710 Act stripped this, replacing it with a statutory term (14+14 years) vesting initially in authors. Fought through litigation (Millar v Taylor, Donaldson v Beckett) and lobbying. Could not exit the trade; adapted by becoming publishers contracting with authors.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, stationers_company, payer,
    powerful, generational, trapped, national).

% Enacted the Statute of Anne (8 Anne c.19) declaring its purpose 'for the Encouragement of Learning.' Defined the category of 'author' as initial proprietor, set term limits, required registration at Stationers' Hall. Collected no direct revenue from the right; legitimacy derived from public purpose framing.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, parliament_state, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Former Stationers' monopoly members who became commercial publishers. Benefited from enforceable contracts with authors (clear title, assignable right). Paid royalties to authors — a new cost. Their market power persisted through control of distribution and capital. Exit meant leaving the book trade entirely.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, publishers_booksellers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__first_holding_reading, publishers_booksellers, payer).

% Faced higher book prices initially as publishers passed on royalty costs and maintained cartel pricing. Gained from increased title variety, eventual public domain access, and competitive reprints after term expiry. Could exit by not buying — but literacy and education made books increasingly necessary.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, reading_public, payer,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__first_holding_reading, reading_public, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved the problem of uncontrolled reprinting that discouraged authorship and concentrated trade in a chartered monopoly. Created a clear, assignable property right in authors, enabling a market for literary labor and a public domain after a limited term.
% TRANSFER_FUNCTION: Moves the legal standing to control reproduction from the Stationers' Company (perpetual, registry-based) to authors (statutory, term-limited). Publishers contract with authors for exclusive license; the public gains access after term expiry. The transfer is from monopoly rent to author royalty + public domain.
% ABSENT_VOICES: Scottish and Irish printers (outside English jurisdiction) who developed a competing cheap reprint trade — they would have argued for no monopoly at all. Women authors (largely excluded from professional authorship in 1710) — their entry into the claimant set came later. Colonial subjects — the Act did not extend to colonies, creating a separate extractive dynamic.
% DISAPPEARANCE_RATIONALE: If the author-right recognition vanished overnight, the book trade would revert to printer/publisher monopoly (or chaos of unenforceable claims). Authors would lose legal standing to contract; publishers would reclaim de facto control; the public domain timetable would collapse. The modern publishing contract, royalty system, and term-based public domain all depend on this founding category.
% FOUNDING_PROBLEM: The Stationers' Company's perpetual monopoly suppressed competition, kept prices high, and gave authors no legal claim to their work — discouraging learned writing and concentrating cultural production in a chartered cartel.
% FOUNDING_PROBLEM_CORROBORATION: The Stationers' monopoly was demonstrably broken by 1774 (Donaldson v Beckett). Contemporary critics (e.g., Lord Camden, parliamentary committees) attested the monopoly was gone. However, the arrangement persisted and expanded — term extensions, removal of formalities, global harmonization — all attested by legislative histories and economic analyses from outside the author/publisher beneficiary set (e.g., Macaulay's 1841 speech, Plant 1986 report, Gowers 2006 review).
narrative_ontology:disappearance_verdict(ip_category_emergence__first_holding_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__first_holding_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__first_holding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ip_category_emergence__first_holding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__first_holding_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__first_holding_reading_tests).
:- end_tests(ip_category_emergence__first_holding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.28) because the 1710 Act limited term and required registration — a genuine constraint on Stationers' extraction. It rises over time as term extensions (1842, 1911, 1988) and removal of formalities increased the rent-capture capacity. Suppression starts high (0.65) because the Stationers resisted violently (litigation, lobbying, extra-legal pressure) and the new right required state enforcement against both piracy and Stationers' common-law claims. Suppression falls as the author-right naturalizes and becomes the default legal furniture. Theater ratio rises as the 'encouragement of learning' justification becomes increasingly detached from the actual term lengths and enforcement priorities. Accessibility collapse is high (0.73) because once 'author' becomes a legal category, alternative framings (e.g., printer-as-proprietor, communal knowledge) become legally incoherent. Resistance is high (0.67) initially from Stationers, then from public domain advocates later.
 *
 * PERSPECTIVAL GAP:
 *   From the author seat (moderate power, constrained exit), the 1710 Act looks like a rope — genuine coordination solving the problem of unauthorized reprints. From the Stationers seat (powerful, trapped), it looks like a snare — forced dispossession of a settled commercial property. From the parliamentary seat (institutional, arbitrage), it looks like a scaffold — a transitional measure to break monopoly and seed a public domain. The engine computes these divergences from the structural data; the claimed_type (scaffold) reflects the parliamentary framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Authors are beneficiaries (d ~ 0.25) — the constraint creates their legal standing. Stationers are victims (d ~ 0.85) — the constraint strips their monopoly. Parliament is agenda_setter (d ~ 0.10) — it writes the rule and collects no direct rent. Publishers are dual: they benefit from enforceable contracts with authors but pay royalties (secondary_role: beneficiary/payer, d ~ 0.50). Reading public bears higher prices initially (payer) but gains from competition and eventual public domain (beneficiary) — net d ~ 0.45.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Stationers' monopoly suppressing competition and learning) was live in 1710. The scaffold's sunset (limited term) was structurally real but politically fragile. By 1842 the founding problem was dead (Stationers' monopoly gone) but the arrangement persisted and expanded — classic mandatrophy. The constraint did not vanish; it inverted: the author-right became the new extraction vehicle. The theater_ratio trajectory captures this: the coordination function (learning encouragement) becomes performative cover for rent extension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stationers_monopoly_vs_author_right_boundary,
    'Was the 1710 shift a genuine transfer of extraction from Stationers to authors, or did the Stationers'' monopoly merely mutate into a new extractive form under author-right rhetoric?',
    'Comparative analysis of Stationers'' Company records pre- and post-1710: did the same commercial actors retain control of the trade through new legal forms?',
    'If the Stationers'' monopoly mutated, the constraint''s claimed scaffold function (transitional author protection) is retrospective cover for continued extraction; if genuine transfer, the scaffold classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stationers_monopoly_vs_author_right_boundary, empirical, 'Whether the 1710 reform displaced or reproduced the prior extractive structure.').

omega_variable(
    kernel_reading_identity_first_holding,
    'Does the first-holding reading describe a distinct constraint from the thinkability reading, or are they two observational angles on the same 1710 category emergence?',
    'Test whether the beneficiary/victim sets differ structurally: first-holding emphasizes WHO holds (authors vs Stationers); thinkability emphasizes WHAT is ownable (expression vs ideas). If the sets overlap completely, they are one constraint.',
    'If one constraint, the kernel decomposition is an authoring artifact; if distinct, the family linkage via affects_constraints is warranted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity_first_holding, conceptual, 'Structural independence of first-holding vs thinkability readings of the 1710 kernel.').

omega_variable(
    scaffold_sunset_credibility,
    'Was the author-right recognition genuinely transitional (scaffold) or was the ''limited time'' framing a permanent extraction mechanism from inception?',
    'Trace the legislative history of the 1710 Act''s duration limits and renewal provisions against contemporary pamphlet debates; compare with subsequent term extensions (1842, 1911, 1988).',
    'If the sunset was always nominal, the constraint is a snare from origin; if genuine but later subverted, it is a scaffold that degraded into a tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scaffold_sunset_credibility, preference, 'Whether the scaffold''s transitional justification was sincere or performative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__first_holding_reading, 1710, 1988).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__first_holding_reading, theater_ratio, 1710, 0.18).
narrative_ontology:measurement(ip_c_tr_t1735, ip_category_emergence__first_holding_reading, theater_ratio, 1735, 0.22).
narrative_ontology:measurement(ip_c_tr_t1774, ip_category_emergence__first_holding_reading, theater_ratio, 1774, 0.27).
narrative_ontology:measurement(ip_c_tr_t1842, ip_category_emergence__first_holding_reading, theater_ratio, 1842, 0.31).
narrative_ontology:measurement(ip_c_tr_t1911, ip_category_emergence__first_holding_reading, theater_ratio, 1911, 0.38).
narrative_ontology:measurement(ip_c_tr_t1988, ip_category_emergence__first_holding_reading, theater_ratio, 1988, 0.44).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__first_holding_reading, base_extractiveness, 1710, 0.28).
narrative_ontology:measurement(ip_c_be_t1735, ip_category_emergence__first_holding_reading, base_extractiveness, 1735, 0.35).
narrative_ontology:measurement(ip_c_be_t1774, ip_category_emergence__first_holding_reading, base_extractiveness, 1774, 0.41).
narrative_ontology:measurement(ip_c_be_t1842, ip_category_emergence__first_holding_reading, base_extractiveness, 1842, 0.48).
narrative_ontology:measurement(ip_c_be_t1911, ip_category_emergence__first_holding_reading, base_extractiveness, 1911, 0.55).
narrative_ontology:measurement(ip_c_be_t1988, ip_category_emergence__first_holding_reading, base_extractiveness, 1988, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__first_holding_reading, suppression_requirement, 1710, 0.65).
narrative_ontology:measurement(ip_c_su_t1735, ip_category_emergence__first_holding_reading, suppression_requirement, 1735, 0.58).
narrative_ontology:measurement(ip_c_su_t1774, ip_category_emergence__first_holding_reading, suppression_requirement, 1774, 0.52).
narrative_ontology:measurement(ip_c_su_t1842, ip_category_emergence__first_holding_reading, suppression_requirement, 1842, 0.48).
narrative_ontology:measurement(ip_c_su_t1911, ip_category_emergence__first_holding_reading, suppression_requirement, 1911, 0.45).
narrative_ontology:measurement(ip_c_su_t1988, ip_category_emergence__first_holding_reading, suppression_requirement, 1988, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__first_holding_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ip_category_emergence__first_holding_reading, 0.12).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__synchronic_diachronic_seam).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, copyright_term_extension_1842).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, copyright_term_extension_1911).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, copyright_term_extension_1988).

% DUAL FORMULATION NOTE:
% This constraint (first_holding_reading) and thinkability_reading decompose the ip_category_emergence kernel along orthogonal axes: WHO holds (first-holding) vs WHAT is ownable (thinkability). The synchronic_diachronic_seam reading tests whether the two axes are structurally independent. All three share the 1710 temporal anchor but differ in ε and beneficiary/victim structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ip_category_emergence__first_holding_reading, moderate, 0.25).
constraint_indexing:directionality_override(ip_category_emergence__first_holding_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
