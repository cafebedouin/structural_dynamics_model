% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__first_holding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: ip_category_emergence__first_holding_reading
 *   human_readable: Author as Statutory Rights-Holder (1710 Occupancy Shift)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   In 1710, England's Statute of Anne legally recognizes the author as a
 *   rights-holder in literary works, displacing the Stationers' Company's
 *   perpetual guild monopoly. This reading emphasizes the occupancy shift:
 *   before 1710, the legitimate claimant set consisted of guild-registered
 *   booksellers who treated first printing as conferring indefinite control;
 *   after 1710, statutory authors and their assigns enter that set with
 *   time-limited exclusive rights, enforceable by common law. The
 *   constraint's persistence depends on active enforcement of this new
 *   occupancy boundary: courts must grant remedies to statutory authors
 *   against unauthorized reprinting, and the Stationers must cede their
 *   perpetual claim. This is distinct from the thinkability_reading (whether
 *   'ownable expression' became a coherent legal category) and the
 *   synchronic_diachronic_seam (whether membership shift and conceptual
 *   emergence are formally independent or temporal artifacts of measurement
 *   framing).
 *
 * KEY AGENTS:
 *   - statutory_authors: beneficiaries of the new claimant status (power: moderate, exit: mobile)
 *   - stationers_company: losers of perpetual monopoly authority (power: powerful, exit: constrained)
 *   - literary_creators: authors not guild-members, now within the legitimate set (power: moderate, exit: mobile)
 *   - printers_without_privilege: newly liable for uniform statutory infringement (power: powerless, exit: trapped)
 *   - crown_parliament: agenda-setter; legislates and enforces the new framework (power: institutional, exit: arbitrage)
 *   - reading_public: trades short-term monopoly cost for long-term public-domain access (power: organized, exit: mobile, secondary role: payer)
 *   - courts_of_law: observer-seat that operationalizes the occupancy shift through remedies and interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, 0.62).
domain_priors:suppression_score(ip_category_emergence__first_holding_reading, 0.48).
domain_priors:theater_ratio(ip_category_emergence__first_holding_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__first_holding_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__first_holding_reading, "Author as Statutory Rights-Holder (1710 Occupancy Shift)").
narrative_ontology:topic_domain(ip_category_emergence__first_holding_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__first_holding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__first_holding_reading, '0e4aa698-9509-49d1-b91b-de0d566a98c8').
narrative_ontology:cs_kernel_codification('0e4aa698-9509-49d1-b91b-de0d566a98c8', formalized).
narrative_ontology:cs_authority_grounding('0e4aa698-9509-49d1-b91b-de0d566a98c8', lineage).
narrative_ontology:cs_interpretation_layer_present('0e4aa698-9509-49d1-b91b-de0d566a98c8').
narrative_ontology:cs_reading_relation('0e4aa698-9509-49d1-b91b-de0d566a98c8', ip_category_emergence__thinkability_reading, influences).
narrative_ontology:cs_reading_relation('0e4aa698-9509-49d1-b91b-de0d566a98c8', ip_category_emergence__synchronic_diachronic_seam, coexists_with).
narrative_ontology:cs_axiom('0e4aa698-9509-49d1-b91b-de0d566a98c8', foundational, authorship_as_primary_source_of_rights).
narrative_ontology:cs_axiom_status(authorship_as_primary_source_of_rights, holdable).
narrative_ontology:cs_axiom_grounding('0e4aa698-9509-49d1-b91b-de0d566a98c8', authorship_as_primary_source_of_rights, deontological).
narrative_ontology:cs_axiom('0e4aa698-9509-49d1-b91b-de0d566a98c8', secondary, monopoly_rent_justified_by_creation_incentive).
narrative_ontology:cs_axiom_status(monopoly_rent_justified_by_creation_incentive, holdable).
narrative_ontology:cs_axiom_grounding('0e4aa698-9509-49d1-b91b-de0d566a98c8', monopoly_rent_justified_by_creation_incentive, instrumental).
narrative_ontology:cs_reference_frame('0e4aa698-9509-49d1-b91b-de0d566a98c8', author_as_statutory_rights_holder).
narrative_ontology:cs_drift_state('0e4aa698-9509-49d1-b91b-de0d566a98c8', donaldson_v_beckett_moment_1774, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('0e4aa698-9509-49d1-b91b-de0d566a98c8', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__first_holding_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, statutory_authors).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, literary_creators).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, stationers_company).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, printers_without_privilege).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, reading_public).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, reading_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors of literary and dramatic works gain a new legal standing post-1710: they become recognized rights-holders in their own work product, with statutory remedies against unauthorized reprinting. Before 1710, they held only personal contractual relationships with booksellers; after, they hold an in-rem claim against the public. This entry into the legitimate claimant set grants them standing in courts of law and access to common-law remedies previously unavailable.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, statutory_authors, beneficiary,
    moderate, generational, mobile, national).

% The Company held a guild monopoly on printing and bookselling in England, enforced by royal patent and internal regulation. The shift to author-as-rights-holder displaces the Company's claim that perpetual control of a work followed from first registration and repeated privilege grants. Post-1710, they must compete for title against authors and their assigns; their monopoly authority erodes as statutory term-limited protection (14 years, then 14 more) replaces their indefinite guild control. They bear the cost of accommodation and reduced monopoly rent.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, stationers_company, payer,
    powerful, generational, constrained, national).

% Playwrights, poets, and authors who were not members of the Stationers' Company gain a statutory right to control first publication and subsequent reprinting during the statutory term. Previously, any work printed by a Stationer and registered with the Company was treated as the Company's perpetual property; the author's recourse was negotiation or the value of the manuscript sale. Post-1710, the law recognizes their interest as primary and time-limited.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, literary_creators, beneficiary,
    moderate, biographical, mobile, national).

% Printers and booksellers outside London (or without Stationers' Company membership) lose the informal tolerance they sometimes enjoyed under Company monopoly negotiation. Under the new regime, they are uniform statutory infringers if they reprint without license from the recognized rights-holder (now the author or author's assignee), with no appeal to Company custom or regional exemption. They face uniform legal liability.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, printers_without_privilege, payer,
    powerless, biographical, trapped, regional).

% Parliament enacts the Statute of Anne (1710) and subsequent copyright legislation. The crown's interest is in establishing a framework that incentivizes literary production while limiting the Stationers' monopoly rent and monopoly rent extraction. This shift reflects a new theory of property: creation (authorship) rather than first reproduction (entry in Company register) becomes the source of rights. Parliament administers and enforces the framework through courts.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, crown_parliament, agenda_setter,
    institutional, civilizational, arbitrage, national).

% The public gains a future interest: after the statutory term, works enter the public domain, available for reprinting at competitive prices. During the term, however, they pay higher prices enforced by the author's exclusive right. The statute trades short-term monopoly extraction for long-term access. They benefit from incentivized creation and eventual free availability; they pay monopoly rents during the grant period.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, reading_public, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__first_holding_reading, reading_public, payer).

% Scotland, other European jurisdictions, and unregulated printing centers abroad are structurally excluded from setting England's copyright regime, though they compete to print English-authored works at lower cost (unrepressed by English law within their borders). Their exclusion is structural, not legal; English enforcement machinery cannot reach them, but English authors cannot easily access their reprinting freedom either.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, rival_jurisdictions, excluded,
    institutional, civilizational, trapped, national).

% Common-law courts (and later statutory courts) adjudicate disputes between claimed authors and alleged infringers. They interpret what 'author' means, whether assignment was valid, whether reprinting was licensed, and what damages apply. They are the mechanism that makes the statutory author-as-rights-holder claim operational; without their willingness to grant remedies, the shift in occupancy is declarative only.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, courts_of_law, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__first_holding_reading, statutory_authors).
narrative_ontology:fixing_cost_class(ip_category_emergence__first_holding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, time-limited property rule in literary works: solves the problem of incentivizing creation and diffusion under a known legal regime, eliminating the need for each author to negotiate separately with monopoly gatekeepers or rely on perpetual monopoly privilege.
% TRANSFER_FUNCTION: Moves exclusive right to reproduce and sell copies from the guild-registered bookseller (Stationer) to the statutory author (or assignee), subject to a term limit (14+14 years initially, then longer). Moves monopoly rent collection from perpetual Company extraction to time-limited author/assignee extraction. After term, works move to public domain and anyone may reprint at competitive price.
% ABSENT_VOICES: Unorganized readers pay higher prices during the monopoly term but have no formal voice in the statute; their interest emerges at the policy level, not at the seat level. Foreign printers and rival jurisdictions are excluded from setting the framework. Future authors (whose interest in a robust precedent is significant) are unrepresented in 1710 negotiations.
% DISAPPEARANCE_RATIONALE: If the author-as-rights-holder framework vanished and the Stationers' monopoly authority were restored, the occupancy of the rights would revert to the first printer/Stationer to register a work, removing statutory term limits and restoring perpetual private monopoly. Literary production incentives would shift, market prices would reflect guild monopoly rather than competitive term-limited rents, and the legal standing of authors would evaporate.
% FOUNDING_PROBLEM: Two pre-1710 problems: (1) The Stationers held perpetual monopoly on reprinting via guild patent and first registration, extracting indefinite rents without compensating authors. (2) Authors had no statutory remedy against unauthorized reprinting; their recourse was contractual negotiation or equitable relief, which was limited and unpredictable.
% FOUNDING_PROBLEM_CORROBORATION: Parliamentary petitions from authors and their assigns attest the founding problem: the Statute of Anne preamble cites incentivizing authorship as the express purpose. Booksellers' own testimony to Parliament (opposing the statute) confirms they held perpetual monopoly and treated author claims as secondary to guild privilege. Historical analysis of pre-1710 publishing disputes shows authors' legal powerlessness. Post-1710 case law (Donaldson v. Beckett, 1774) from the judiciary confirms the shift: the court upholds the statutory author-as-rights-holder principle against the Company's claim to perpetual common-law right.
narrative_ontology:disappearance_verdict(ip_category_emergence__first_holding_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__first_holding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__first_holding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ip_category_emergence__first_holding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__first_holding_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Base extractiveness is 0.62 because the constraint manifests as both coordination (incentivizing authorship with property protection) and extraction (monopoly rents during the grant term). Suppression at 0.48 reflects a critical shift: pre-1710 suppression was very high (0.72) because the Stationers' monopoly was enforced by guild exclusion and royal privilege with no legal recourse for authors. Post-1710 suppression drops because the framework explicitly recognizes author rights and provides statutory remedies; the suppressive machinery of guild monopoly is dismantled in law, though de facto market concentration persists. Theater ratio is low (0.22 by 1774) because the core function—incentivizing creation and managing reprinting—is operational; theatrical compliance (rent-justifying talk) increases slightly over time as the Stationers adapt their rhetoric to statutory language. Accessibility collapse declines from 0.89 to 0.68 (structural level) as alternatives emerge: post-1710, authors can assign their rights, can publish directly (though mechanics were limited), and can litigate without guild sponsorship. The one-shared-grid rule applies: all metrics (base_extractiveness, suppression, theater_ratio) are authored at every time point (1690, 1710, 1730, 1750, 1774), and the coercion grid captures level-differentiated dynamics at the two interval endpoints only.
 *
 * PERSPECTIVAL GAP:
 *   The Stationers' seat and the statutory authors' seat compute a wide divergence. From the Stationers' perspective, the arrangement is an unjust taking of their legitimately held monopoly property; they experience it as a sudden confiscation of their capital value (the perpetual right to control reprints of registered works). From the authors' perspective, the arrangement restores their primary claim and removes the intermediary tax: they experience it as enabling direct rights-holding and market participation. From the courts' perspective, it is a new legal framework requiring interpretation and boundary-setting—a novel problem of statutory construction. The engine computes this divergence from the structural data (beneficiaries vs. victims, power atoms, exit options, time horizons) and should produce a tangled_rope classification from the authors' and creators' seats (coordination + asymmetric extraction), but from the Stationers' seat the classification should reflect their experience of loss and constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Statutory authors and literary creators are structural beneficiaries (d → 0.0 to 0.3, near full-beneficiary end): they enter the claimant set, gain enforceable rights, and access courts previously closed to them. Extraction χ for them is low or negative (subsidy). The Stationers' Company is a structural victim (d → 0.7 to 1.0, near full-target end): their perpetual monopoly is converted to a shared legal landscape where authors can compete for rights. Their exit options are constrained (they cannot exit publishing; they must adapt); their power is powerful but concentrated in a now-challenged monopoly. The reading public sits near symmetric (d → 0.45 to 0.55): they benefit from incentivized creation and eventual public-domain access; they pay monopoly rents during the statutory term. Printers without privilege are victims of a different sort (d → 0.8 to 1.0): they go from informal tolerance (negotiation with Stationers) to legal liability (uniform statutory infringement), with no compensating benefit. The coercion grid captures this across four levels: at the individual level (unorganized author or printer) accessibility is high pre-1710 (trapped), moderate post-1710 (can negotiate or litigate with support). At the organizational level (guild, association, assignee networks) accessibility is better post-1710 (can organize around author rights). At the class level (all authors, all printers) the structural shift is evident. At the structural level (the legal system itself) alternatives collapse for the Stationers but open for authors.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy collapse because the founding problem (authors lack rights; Stationers extract perpetual rents without compensating creation) remains live and addressed by the statutory solution. Post-1710 cases like Donaldson v. Beckett (1774) re-litigate the scope and term of author rights but do not question the principle that authors hold primary rights. If the constraint were to decay into theater (rents collected via author-assignment without ongoing creation incentive), that would signal mandatrophy—the function (incentivizing creation) would have atrophied while the transfer (extracting monopoly rents) persisted. The measurement series shows theater_ratio rising modestly from 0.08 to 0.22, suggesting some theatrical maintenance of the 'incentive' narrative over time, but not yet mandatrophic collapse. The constraint remains functionally live (courts grant remedies, authors do publish and assign rights, readers do benefit from new work), so mandatrophy_resolved remains false.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    occupancy_vs_category_independence,
    'Is the occupancy shift (authors entering the legitimate claimant set) formally independent of the category emergence (ownable expression becoming thinkable as property), or is occupancy change merely the observable consequence of prior conceptual change?',
    'Close reading of pre-1710 legal texts to determine whether authors were conceived as potential rights-holders before the statute, or whether the statute introduced the concept de novo. Examine whether petitions to Parliament reveal authors as an unrecognized constituency with latent standing, or whether they were not yet a conceptual category in law.',
    'If independent, the first_holding_reading and thinkability_reading describe distinct constraints with distinct ε values and temporal profiles. If dependent (occupancy follows necessarily from thinkability), they are a single constraint measured at different analytical levels, and one ε should cover both.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupancy_vs_category_independence, conceptual, 'Whether membership-set shift and legal-category emergence are independent constraints or aspects of one constraint.').

omega_variable(
    enforcement_beneficiary_drift,
    'Did the enforcement beneficiary remain constant (always the dominant printing entity with power to suppress rivals), or did it genuinely shift from Stationers-collective to authors-as-dispersed-beneficiaries?',
    'Examine post-1710 infringement litigation: who brings cases? Do authors or their assignees become the primary enforcers (via common-law actions), or do the Stationers continue to enforce on their behalf? Track the shift in case parties and remedies sought over the 1710–1774 interval.',
    'If enforcement beneficiary shifted to authors, the constraint''s persistence depends on dispersed individual authors maintaining vigilance—a fragile cooperative arrangement. If Stationers remained the primary enforcers (acting as authors'' agents), the constraint is more akin to a stable but transferred monopoly, and extraction remains concentrated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_beneficiary_drift, empirical, 'Whether enforcement machinery migrated from monopoly gatekeepers to distributed rights-holders.').

omega_variable(
    statutory_term_as_boundary_commitment,
    'Is the 14+14-year statutory term (later extended) a genuine boundary-commitment that Parliament and courts respect, or a provisional statement that has been repeatedly extended in practice, converting into indefinite de facto protection?',
    'Trace statutory amendments and judicial enforcement of the term limit from 1710 to the present. Measure whether terms have been reset/extended before expiration, whether courts have enforced term limits, and whether the public domain has actually received works at term''s end.',
    'If terms are genuinely respected, the constraint is tangled_rope with a defined time limit, and extraction is transient. If terms are perpetually reset before expiration, the constraint drifts toward a snare (extraction without sunset), and the founding problem (incentivizing creation) becomes cover for perpetual monopoly rents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statutory_term_as_boundary_commitment, empirical, 'Whether the statutory term limit is operationally binding or has drifted toward indefinite effective protection.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the post-1710 decline in measured suppression (from 0.72 to 0.38) driven by structural dismantling of guild monopoly enforcement, or by internalization (authors adopt the enforcement burden themselves, reducing the observable suppression by shifting it to a diffuse distributed enforcement regime)?',
    'Post-1710 suppression trajectory: if suppression remains low when authors have the right to enforce (suggesting they do enforce via litigation or negotiation), the mechanism is internalized. If suppression rises whenever author-enforcement effort flags, the mechanism is structural (the removal of guild gatekeeping genuinely reduces coercive pressure).',
    'If internalized, the constraint''s apparent ''softening'' masks a shift of enforcement burden from institutional monopoly to dispersed individual authors—suppression may reappear when authors are unable or unwilling to enforce. If structural, the constraint genuinely becomes less coercive as alternatives emerge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression decline reflects genuine institutional loosening or burden-shifting to distributed enforcers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__first_holding_reading, 1690, 1774).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1690, ip_category_emergence__first_holding_reading, theater_ratio, 1690, 0.08).
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__first_holding_reading, theater_ratio, 1710, 0.15).
narrative_ontology:measurement(ip_c_tr_t1730, ip_category_emergence__first_holding_reading, theater_ratio, 1730, 0.18).
narrative_ontology:measurement(ip_c_tr_t1750, ip_category_emergence__first_holding_reading, theater_ratio, 1750, 0.22).
narrative_ontology:measurement(ip_c_tr_t1774, ip_category_emergence__first_holding_reading, theater_ratio, 1774, 0.22).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1690, ip_category_emergence__first_holding_reading, base_extractiveness, 1690, 0.55).
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__first_holding_reading, base_extractiveness, 1710, 0.62).
narrative_ontology:measurement(ip_c_be_t1730, ip_category_emergence__first_holding_reading, base_extractiveness, 1730, 0.64).
narrative_ontology:measurement(ip_c_be_t1750, ip_category_emergence__first_holding_reading, base_extractiveness, 1750, 0.68).
narrative_ontology:measurement(ip_c_be_t1774, ip_category_emergence__first_holding_reading, base_extractiveness, 1774, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1690, ip_category_emergence__first_holding_reading, suppression_requirement, 1690, 0.72).
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__first_holding_reading, suppression_requirement, 1710, 0.48).
narrative_ontology:measurement(ip_c_su_t1730, ip_category_emergence__first_holding_reading, suppression_requirement, 1730, 0.45).
narrative_ontology:measurement(ip_c_su_t1750, ip_category_emergence__first_holding_reading, suppression_requirement, 1750, 0.42).
narrative_ontology:measurement(ip_c_su_t1774, ip_category_emergence__first_holding_reading, suppression_requirement, 1774, 0.38).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1690, tn=1774
narrative_ontology:measurement(ip_c_grid_01, ip_category_emergence__first_holding_reading, accessibility_collapse(class), 1690, 0.85).
narrative_ontology:measurement(ip_c_grid_02, ip_category_emergence__first_holding_reading, accessibility_collapse(class), 1774, 0.72).
narrative_ontology:measurement(ip_c_grid_03, ip_category_emergence__first_holding_reading, accessibility_collapse(individual), 1690, 0.88).
narrative_ontology:measurement(ip_c_grid_04, ip_category_emergence__first_holding_reading, accessibility_collapse(individual), 1774, 0.65).
narrative_ontology:measurement(ip_c_grid_05, ip_category_emergence__first_holding_reading, accessibility_collapse(organizational), 1690, 0.92).
narrative_ontology:measurement(ip_c_grid_06, ip_category_emergence__first_holding_reading, accessibility_collapse(organizational), 1774, 0.58).
narrative_ontology:measurement(ip_c_grid_07, ip_category_emergence__first_holding_reading, accessibility_collapse(structural), 1690, 0.89).
narrative_ontology:measurement(ip_c_grid_08, ip_category_emergence__first_holding_reading, accessibility_collapse(structural), 1774, 0.68).
narrative_ontology:measurement(ip_c_grid_09, ip_category_emergence__first_holding_reading, resistance(class), 1690, 0.32).
narrative_ontology:measurement(ip_c_grid_10, ip_category_emergence__first_holding_reading, resistance(class), 1774, 0.58).
narrative_ontology:measurement(ip_c_grid_11, ip_category_emergence__first_holding_reading, resistance(individual), 1690, 0.35).
narrative_ontology:measurement(ip_c_grid_12, ip_category_emergence__first_holding_reading, resistance(individual), 1774, 0.62).
narrative_ontology:measurement(ip_c_grid_13, ip_category_emergence__first_holding_reading, resistance(organizational), 1690, 0.28).
narrative_ontology:measurement(ip_c_grid_14, ip_category_emergence__first_holding_reading, resistance(organizational), 1774, 0.68).
narrative_ontology:measurement(ip_c_grid_15, ip_category_emergence__first_holding_reading, resistance(structural), 1690, 0.25).
narrative_ontology:measurement(ip_c_grid_16, ip_category_emergence__first_holding_reading, resistance(structural), 1774, 0.62).
narrative_ontology:measurement(ip_c_grid_17, ip_category_emergence__first_holding_reading, stakes_inflation(class), 1690, 0.75).
narrative_ontology:measurement(ip_c_grid_18, ip_category_emergence__first_holding_reading, stakes_inflation(class), 1774, 0.42).
narrative_ontology:measurement(ip_c_grid_19, ip_category_emergence__first_holding_reading, stakes_inflation(individual), 1690, 0.78).
narrative_ontology:measurement(ip_c_grid_20, ip_category_emergence__first_holding_reading, stakes_inflation(individual), 1774, 0.48).
narrative_ontology:measurement(ip_c_grid_21, ip_category_emergence__first_holding_reading, stakes_inflation(organizational), 1690, 0.82).
narrative_ontology:measurement(ip_c_grid_22, ip_category_emergence__first_holding_reading, stakes_inflation(organizational), 1774, 0.35).
narrative_ontology:measurement(ip_c_grid_23, ip_category_emergence__first_holding_reading, stakes_inflation(structural), 1690, 0.8).
narrative_ontology:measurement(ip_c_grid_24, ip_category_emergence__first_holding_reading, stakes_inflation(structural), 1774, 0.38).
narrative_ontology:measurement(ip_c_grid_25, ip_category_emergence__first_holding_reading, suppression(class), 1690, 0.82).
narrative_ontology:measurement(ip_c_grid_26, ip_category_emergence__first_holding_reading, suppression(class), 1774, 0.38).
narrative_ontology:measurement(ip_c_grid_27, ip_category_emergence__first_holding_reading, suppression(individual), 1690, 0.85).
narrative_ontology:measurement(ip_c_grid_28, ip_category_emergence__first_holding_reading, suppression(individual), 1774, 0.42).
narrative_ontology:measurement(ip_c_grid_29, ip_category_emergence__first_holding_reading, suppression(organizational), 1690, 0.88).
narrative_ontology:measurement(ip_c_grid_30, ip_category_emergence__first_holding_reading, suppression(organizational), 1774, 0.35).
narrative_ontology:measurement(ip_c_grid_31, ip_category_emergence__first_holding_reading, suppression(structural), 1690, 0.88).
narrative_ontology:measurement(ip_c_grid_32, ip_category_emergence__first_holding_reading, suppression(structural), 1774, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__first_holding_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ip_category_emergence__first_holding_reading, 0.12).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% The kernel ip_category_emergence decomposes into three structurally distinct constraints, each with a different referent and ε value. The first_holding_reading (this story) focuses on occupancy shift in the claimant set and the change in enforcement beneficiary from Stationers to statutory authors. The thinkability_reading focuses on whether ownable expression became a coherent legal category (independent of who holds the rights). The synchronic_diachronic_seam contests whether these two readings describe independent constraints or temporal artifacts of the same underlying event. Each reading produces a different ε and a different terminal classification; they are linked via network.affects_constraints to indicate constraint-family kinship and to enable contamination-propagation analysis. No single ε covers all three; divergence in measured types across readings is expected and diagnostic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ip_category_emergence__first_holding_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
