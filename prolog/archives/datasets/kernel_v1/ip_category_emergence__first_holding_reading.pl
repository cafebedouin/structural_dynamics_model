% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__first_holding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-06-15
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
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ip_category_emergence__first_holding_reading
 *   human_readable: IP Category Emergence: Author as Rights-Holder (First Holding Reading)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   The Statute of Anne (1710) instantiated a categorical transformation in
 *   English law: the 'author' entered the legitimate claimant set for
 *   intellectual property rights. Before 1710, English printing was governed
 *   by the Stationers' Company, a guild monopoly that claimed perpetual
 *   copyright in the books they entered in the Register. The occupant of the
 *   copyright claim was the printer/stationer, not the author. The author had
 *   no legal right to their work; compensation (if any) came through
 *   patronage or negotiated fees to the printer. The Statute of Anne reversed
 *   this: it declared that the author 'shall have the sole right and liberty
 *   of printing' for a fixed term (14 years, renewable once for another 14 if
 *   the author still lived). The statute thus redefined the initial holder of
 *   the copyright from printer-as-guild-member to
 *   author-as-individual-proprietor. This reading traces the constraint as
 *   the structural emergence of a new category: author-as-rights-holder. It
 *   is distinct from the 'thinkability reading' (which would emphasize the
 *   conceptual preconditions for authors to be thinkable as legal persons)
 *   and the 'synchronic-diachronic seam reading' (which would emphasize the
 *   moment-of-shift as technically located between old and new institutional
 *   frameworks). This reading emphasizes occupancy change: who is now
 *   authorized to claim copyright, and what this redistribution of legal
 *   standing means for the actors it displaces and the coordination problems
 *   it solves.
 *
 * KEY AGENTS:
 *   - Author as Individual Proprietor (powerful/arbitrage): Newly entered claimant set in 1710; now holds initial copyright right before assigning to publisher. Captures new form of economic and legal value.
 *   - Printer/Stationer Guild Member (powerless/trapped): Pre-1710 occupant of copyright claim via guild membership. Dispossessed by statutory reclassification without compensation. Cannot exit the framework shift.
 *   - Publisher as Contractual Licensee (institutional/constrained): Mid-position: gains from expanded publication incentives and statutory anti-piracy protection, but loses direct monopoly authority and must negotiate with authors.
 *   - Reading Public / Libraries (organized/constrained): Benefits from expanded publication incentive structure; faces temporary restriction (14-year term) but eventual access to commons.
 *   - Stationers' Company as Institution (institutional/constrained): The guild's legal status is reclassified from monopoly-holder to derivative licensee. Faces institutional reorganization but retains capacity to negotiate with authors as a collective.
 *   - Legislative Reform Coalition (organized/mobile): Whig parliamentary majority, printer-authors (Defoe), London booksellers seeking to break monopoly. Retains agency over statute revision and sunset.
 *   - Analytical Observer (analytical/analytical): Sees the constraint as both coordination (solving book-trade investment) and extraction (dispossessing guild occupants, restricting reading access temporarily). Tangled Rope from civilizational scope.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, 0.52).
domain_priors:suppression_score(ip_category_emergence__first_holding_reading, 0.48).
domain_priors:theater_ratio(ip_category_emergence__first_holding_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__first_holding_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__first_holding_reading, "IP Category Emergence: Author as Rights-Holder (First Holding Reading)").
narrative_ontology:topic_domain(ip_category_emergence__first_holding_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__first_holding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__first_holding_reading, 'e6d6a640-57db-4086-be3a-23007839d550').
narrative_ontology:cs_kernel_codification('e6d6a640-57db-4086-be3a-23007839d550', formalized).
narrative_ontology:cs_authority_grounding('e6d6a640-57db-4086-be3a-23007839d550', extraction).
narrative_ontology:cs_interpretation_layer_present('e6d6a640-57db-4086-be3a-23007839d550').
narrative_ontology:cs_reading_relation('e6d6a640-57db-4086-be3a-23007839d550', ip_category_emergence__thinkability_reading, influences).
narrative_ontology:cs_reading_relation('e6d6a640-57db-4086-be3a-23007839d550', ip_category_emergence__synchronic_diachronic_seam_reading, coexists_with).
narrative_ontology:cs_axiom('e6d6a640-57db-4086-be3a-23007839d550', foundational, author_occupancy_is_statutory_creation).
narrative_ontology:cs_axiom_status(author_occupancy_is_statutory_creation, holdable).
narrative_ontology:cs_axiom_grounding('e6d6a640-57db-4086-be3a-23007839d550', author_occupancy_is_statutory_creation, deontological).
narrative_ontology:cs_axiom('e6d6a640-57db-4086-be3a-23007839d550', foundational, occupancy_change_is_extraction_mechanism).
narrative_ontology:cs_axiom_status(occupancy_change_is_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('e6d6a640-57db-4086-be3a-23007839d550', occupancy_change_is_extraction_mechanism, instrumental).
narrative_ontology:cs_reference_frame('e6d6a640-57db-4086-be3a-23007839d550', guild_monopoly_perpetual_copyright).
narrative_ontology:cs_drift_state('e6d6a640-57db-4086-be3a-23007839d550', post_statute_anne_1710, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('e6d6a640-57db-4086-be3a-23007839d550', '2025-06-15T14:32:00Z').
narrative_ontology:cs_kernel_id(ip_category_emergence__first_holding_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, authors_as_individual_proprietors).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, publishers_under_statutory_grant).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, printers_guild_monopoly_holders).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, public_reading_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPOSSESSED PRINTER (SNARE) — The pre-1710 occupant of the Stationers' monopoly faced sudden legal reclassification. The printer had legitimate claim as a guild member under customary law; the Statute of Anne reclassified the right as inalienably vested in the author. The printer cannot exit — the category shift is instantaneous and enforced. Maximum extraction from their perspective: loss of inherited guild privilege without compensation. Trapped in the old framework while the framework itself is redefined.
constraint_indexing:constraint_classification(ip_category_emergence__first_holding_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AUTHOR AS INDIVIDUAL PROPRIETOR (ROPE) — The author enters the legitimate claimant set newly minted in 1710. They experience the constraint as pure coordination: the statutory grant from author (initial holder) to publisher (transferee) solves the problem of securing books against unauthorized reprinting while giving the author a property right that can be alienated. No extraction from the author's perspective — they capture a new form of value and retain arbitrage (can license to multiple publishers, choose who to deal with, or refuse publication entirely). This is their framework; they have full agency.
constraint_indexing:constraint_classification(ip_category_emergence__first_holding_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLISHER (TANGLED ROPE) — Publishers occupy the mid-position. They benefit from statutory protection against unauthorized reprinting (coordination function: authors can now safely invest in publication, publishers can acquire limited copyright via contractual grant). But they also face new constraints: they must negotiate with authors (loss of direct guild-monopoly authority), manage the license term (14 years), and operate under the legal fiction that they hold a delegated right, not an inherited privilege. Mixed: genuine coordination (incentive to publish expands the book trade), genuine extraction (loss of perpetual monopoly, subordination to author's initial claim).
constraint_indexing:constraint_classification(ip_category_emergence__first_holding_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: READING PUBLIC (ROPE) — The reading public gains from expanded publication incentives: more authors write when they can hold a property right; more publishers invest when they can secure copies against piracy. The constraint exhibits low extraction from this perspective because the coordination function (securing investment in book production) delivers genuine public benefit. The 14-year term provides eventual access to the commons. Suppression is real (can't legally reproduce during the term) but not maximal — it's bounded and eventually expires. Organized public actors (libraries, copyists' guilds, booksellers seeking to reprint expired works) can organize around the term boundaries.
constraint_indexing:constraint_classification(ip_category_emergence__first_holding_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATIONERS' COMPANY (SNARE) — The guild structure faces institutional reclassification. Members held monopoly rights through inherited guild membership and entrance fees. The Statute reframes their holdings as derivative — they now hold only what authors grant them contractually. The guild cannot exit because its legal status is unilaterally redefined by Parliament. This is extraction relative to their pre-statute position, but the classification differs from the dispossessed printer because the guild as an institution has greater capacity to reorganize (they can negotiate standardized author contracts, establish clearinghouses, form consortia). But the core mechanism is snare-like: forced exit from the prior category with no compensation.
constraint_indexing:constraint_classification(ip_category_emergence__first_holding_reading, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGISLATIVE REFORM COALITION (SCAFFOLD) — The coalition that authored the Statute of Anne (Whig parliamentary majorities, printer-authors like Daniel Defoe, London booksellers seeking to break guild monopoly) sees the category shift as a sunset clause on guild monopoly: the statute is temporary, revisable, and designed to be superseded as book culture matures. The coalition has agency and explicitly framed this as a limited grant. The statute itself declares terms (14+14 years, then reversion to commons). This perspective sees low extraction because the reform has a declared endpoint and the coalition retains capacity to revise.
constraint_indexing:constraint_classification(ip_category_emergence__first_holding_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the Statute of Anne instantiates a genuine category shift: the legal concept 'author' enters the set of persons capable of holding property rights in the fruits of intellectual labor. This is coordination (solving the book-trade investment problem by creating incentive structure). It is also extraction (the old guild occupants are dispossessed without compensation; the printing commons is restricted for 14 years). The constraint is Tangled Rope because both functions are structural and cannot be separated — the coordination benefit IS enabled by the extraction mechanism (legal monopoly on reprinting secures publisher investment).
constraint_indexing:constraint_classification(ip_category_emergence__first_holding_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__first_holding_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ip_category_emergence__first_holding_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ip_category_emergence__first_holding_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ip_category_emergence__first_holding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint creates genuine coordination (authors now have incentive to write and publish; publishers have incentive to invest in high-quality editions knowing they can sue for piracy). This is real coordination value. But it is achieved through extraction: the guild occupants (printers who held perpetual copyright via guild membership) are dispossessed without compensation. The public's reading rights are temporarily restricted (14-year monopoly on reprinting). The extractiveness value reflects the net: genuine coordination function exists, but it is enabled by and inseparable from extraction of prior rights. If the statute had provided compensatory licensing or transition mechanisms, extractiveness would be lower. Suppression (0.48): Moderate. Barriers to reprinting during the copyright term are legal (statutory monopoly, enforcement via courts), not physically insurmountable. Publishers enforce through litigation, but they face costs and uncertainty. The suppression is not total because: (1) the term is limited; (2) fair use and scholarly reprinting have some space (not formalized in 1710 but practiced); (3) enforcement is expensive, so many minor infringements go unpunished. Theater ratio (0.38): Low-moderate. The statute is relatively low in theater compared to guild monopoly because the statutory mechanism is explicit and rule-based (14-year term, author must be alive for renewal, term then expires). The coordination function is transparent: authors and publishers can both see how the incentive structure works. There is less performative mystification than the guild monopoly required (guild membership ritual, Register entry theater, perpetual inheritance claims dressed in customary law language). The statute's theater is reduced because its mechanism is codified and revisable by Parliament.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal: the author and legislative coalition see Rope/Scaffold (pure coordination, sunset mechanism, agency), while the dispossessed printer and stationers see Snare (forced exit, reclassification, no compensation). Publishers see Tangled Rope (mixed benefit and constraint). The gap reflects a fundamental shift in which actors occupy the legitimate claimant set. The reading's analytical contribution is precisely this gap: the constraint is not a single 'law' but a redistribution of legal standing that appears as coordination to some (authors, reformers) and extraction to others (guild occupants). The gap cannot be closed by better information — it reflects genuine structural conflict over who has the right to claim copyright.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position and exit options. For the author (powerful/arbitrage), d ≈ 0.05 (full beneficiary with maximal exit options — can choose to publish or not, can license to multiple publishers, can refuse bad deals). For the printer (powerless/trapped), d ≈ 1.0 (full target with no exit — sudden reclassification with no compensatory mechanism). For the publisher (institutional/constrained), d ≈ 0.55 (mixed position: benefits from statutory protection, constrained by need to negotiate with authors and respect the 14-year boundary; gains some, loses some relative to pure guild membership). For the public (organized/constrained), d ≈ 0.45 (constrained by 14-year term, benefited by expanded publishing incentive; more benefit than cost over their lifetime). For the guild as institution (institutional/constrained), d ≈ 0.72 (strong target position: loses institutional legitimacy, must reorganize, but retains capacity to adapt). The analytical observer uses canonical d for analytical context ≈ 0.73. The directionality profile shows asymmetric extraction (high d for dispossessed printer, moderate d for institutional actors, low d for author beneficiary), which supports the Tangled Rope classification: both coordination and extraction functions are structural.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by explicitly acknowledging that the category shift creates both coordination and extraction, and that these are structurally inseparable. The statute solves the book-trade investment problem by creating author property and publisher licensing — genuine coordination. But it does so by unilaterally reclassifying the occupant of the right from printer to author, dispossessing guild members without compensation — genuine extraction. The mandatrophy does not dissolve if we call it 'really coordination' or 'really extraction' — the constraint genuinely instantiates both. The resolution is accepting that institutional innovations routinely exhibit this dual character: they solve a coordination problem by restructuring who holds which rights, and that restructuring always extracts from the prior occupants. The constraint is thus a true Tangled Rope: requires active enforcement (statutory monopoly on reprinting), has genuine beneficiaries (authors, publishers, reading public), has genuine victims (guild occupants, temporarily restricted readers), and cannot be decomposed into pure coordination + separate extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    guild_occupancy_vs_statutory_fiction,
    'Is the author''s property right in the Statute a genuine new category, or a legal fiction imposed retroactively on existing guild monopoly holdings?',
    'Historical analysis of pre-statute author compensation practices; examination of whether stationers explicitly claimed ''authorial'' property before 1710 vs implicit property in their copies. Textual analysis of the statute''s language (whose ''right'' is it really?). Track author earnings data before and after 1710.',
    'If fiction: the constraint is pure expropriation disguised as category expansion (Snare, not Tangled Rope). If genuine: the constraint solves a coordination problem that guild monopoly could not (Tangled Rope as classified). The reading depends on whether the statute creates author property or redistributes guild property.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(guild_occupancy_vs_statutory_fiction, empirical, 'Whether author property is genuine category creation or legal fiction masking redistribution').

omega_variable(
    enforcement_beneficiary_shift,
    'Who actually benefits from enforcement: the author as initial holder, or the publisher as assignee? Which party bears enforcement costs?',
    'Patent and licensing records post-1710; litigation patterns (who sues for infringement, who pays for litigation). Analysis of contract terms between authors and publishers (what share of enforcement cost burden is contractually allocated). Compare pre-statute guild enforcement cost to post-statute statutory enforcement structure.',
    'If authors benefit most and bear costs: author-as-rights-holder reading is accurate. If publishers benefit most and authors bear only nominal benefit: the constraint is disguised publisher-cartel capture (the statute replaces guild monopoly with publisher-backed copyright). The reading''s foundation depends on the true beneficiary of enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_beneficiary_shift, empirical, 'Whether author or publisher is the true beneficiary of statutory enforcement').

omega_variable(
    category_permanence_vs_contingency,
    'Is the author''s entry into the legitimate claimant set a permanent institutional shift, or was it contingent on the political coalition that authored the statute?',
    'Track subsequent extension of copyright terms (Statute of Anne extended 1814, 1870, 1976, etc.). Examine whether subsequent legislation attempted to revert author property to guild/publisher control (it did not). Analyze the ideological durability of the ''author'' category across regime changes (1689 Glorious Revolution framing, later Romantic authorship). Check whether stationers ever successfully petitioned to reclaim pre-statute monopoly (failed).',
    'If contingent: the reading instantiates a specific political moment rather than a structural category shift (Scaffold with an exceptionally long sunset). If permanent: author-as-rights-holder is truly a new category (Tangled Rope). The classification hinges on the stability of the institutional innovation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(category_permanence_vs_contingency, empirical, 'Whether author property is permanent institutional shift or contingent political outcome').

omega_variable(
    comparative_institutional_analysis,
    'How does the Statute of Anne''s category shift compare structurally to other concurrent institutional revolutions (e.g., joint-stock company proliferation, landed estate entailment reform, colonial property regimes)? Is author-as-rights-holder a unique category shift or part of a broader pattern of propertization?',
    'Comparative historical analysis of property regimes circa 1700–1750. Examine whether similar category expansions (person types newly capable of holding specific property forms) occurred in commerce, land, and colonial law. Identify structural commonalities and differences.',
    'If unique: the reading reflects a singular creative moment in English law (supports strong reading emphasis on 1710 as hinge). If part of a pattern: the reading is one instance of a broader institutional trend (suggests the category shift is structural, not contingent, but also less exceptional). The classification may shift to or away from Scaffold depending on pattern visibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comparative_institutional_analysis, conceptual, 'Whether author property is unique institutional innovation or instance of broader propertization pattern').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__first_holding_reading, 1709, 1724).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ipcate_theater_1709, ip_category_emergence__first_holding_reading, theater_ratio, 1709, 0.65).
narrative_ontology:measurement(ipcate_theater_1710, ip_category_emergence__first_holding_reading, theater_ratio, 1710, 0.38).
narrative_ontology:measurement(ipcate_theater_1724, ip_category_emergence__first_holding_reading, theater_ratio, 1724, 0.35).

% Extraction over time
narrative_ontology:measurement(ipcate_extract_1709, ip_category_emergence__first_holding_reading, base_extractiveness, 1709, 0.0).
narrative_ontology:measurement(ipcate_extract_1710, ip_category_emergence__first_holding_reading, base_extractiveness, 1710, 0.52).
narrative_ontology:measurement(ipcate_extract_1724, ip_category_emergence__first_holding_reading, base_extractiveness, 1724, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__first_holding_reading, resource_allocation).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__synchronic_diachronic_seam_reading).

% DUAL FORMULATION NOTE:
% The constraint 'ip_category_emergence' decomposes into three distinct readings, each with its own constraint story. All three share the same base historical event (Statute of Anne 1710) but emphasize different structural aspects: occupancy shift (this reading), conceptual preconditions (thinkability_reading), and moment-of-shift location (synchronic_diachronic_seam_reading). Each reading produces its own ε value, its own perspectives, and its own classification because each reading asks a different structural question. This is not observable-dependence (ε-invariance violation) but rather different questions about the same kernel. The three stories are linked as siblings via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
