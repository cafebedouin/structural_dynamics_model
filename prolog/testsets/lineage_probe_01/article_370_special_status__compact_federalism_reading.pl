% ============================================================================
% CONSTRAINT STORY: article_370_special_status__compact_federalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_370_compact_federalism, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: article_370_special_status__compact_federalism_reading
 *   human_readable: Article 370: Compact Federalism Reading (Autonomy as Price of Union)
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   Article 370 of the Indian Constitution grants Jammu & Kashmir a special
 *   status — autonomy in internal governance in exchange for union membership
 *   and central control over defense, foreign affairs, and communications.
 *   The compact-federalism reading interprets this as a binding accession
 *   compact: the center accepted J&K's accession on the condition of
 *   preserving autonomy, and this condition cannot be unilaterally revised by
 *   the center. Autonomy is the price of union, not a gift. On this reading,
 *   the 2019 abrogation (Presidential Order revoking Article 370 and
 *   splitting J&K into union territories) violated the compact because it was
 *   unilateral — the center acted without consent of the entity whose
 *   autonomy was the condition of the original accession. This reading
 *   suppresses the center's authority to revise unilaterally and benefits
 *   federal compact theory. It stands in contest with two sibling readings:
 *   the abrogation_2019_reading (which accepts the abrogation as
 *   constitutionally valid, tested on itself), and the
 *   temporary_provision_reading (which treats Article 370's 'temporary'
 *   designation as inherent to its meaning, making termination always
 *   legitimate). The constraint's theater ratio (0.48 initially, rising to
 *   0.55 by 1990, then stabilizing at 0.48) reflects that the compact logic
 *   has been sustained doctrinally even as the center has eroded J&K's
 *   functional autonomy through statutory measures and administrative
 *   practice. The suppression requirement (0.35 initially, rising to 0.68 by
 *   1990 during the insurgency period) tracks the escalating enforcement
 *   needed to maintain central authority while the doctrinal compact remained
 *   formally intact.
 *
 * KEY AGENTS:
 *   - Compact-Federalism Constitutional Theory: Primary beneficiary (institutional/constrained) — the reading sustains the principle that founding compacts bind all parties and cannot be unilaterally revised
 *   - Jammu & Kashmir Autonomous Governance: Primary victim and partial beneficiary (organized/constrained) — on this reading, the entity has a binding right to autonomy, but that right is constrained by union membership and dependent on the center's recognition
 *   - Integrationist Constitutionalism: Primary victim (powerless/trapped) — the reading suppresses the logic of progressive integration toward constitutional uniformity; the integrationist vision cannot exit the compact framework
 *   - Central Authority (Union of India): Mixed beneficiary and victim (institutional/arbitrage) — benefits from union membership but constrained by the compact logic; cannot unilaterally revise accession terms
 *   - Statutory Administrative Practice: Secondary institutional actor (institutional/constrained) — has operationalized erosion of autonomy within the formal compact framework, sustaining theater
 *   - Analytical Federalism Observer: Neutral observer (analytical/analytical) — sees the compact logic as a natural law of federal constitutionalism, but risks naturalizing a contingent historical arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_370_special_status__compact_federalism_reading, 0.38).
domain_priors:suppression_score(article_370_special_status__compact_federalism_reading, 0.62).
domain_priors:theater_ratio(article_370_special_status__compact_federalism_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_370_special_status__compact_federalism_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(article_370_special_status__compact_federalism_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(article_370_special_status__compact_federalism_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_370_special_status__compact_federalism_reading, tangled_rope).
narrative_ontology:human_readable(article_370_special_status__compact_federalism_reading, "Article 370: Compact Federalism Reading (Autonomy as Price of Union)").
narrative_ontology:topic_domain(article_370_special_status__compact_federalism_reading, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(article_370_special_status__compact_federalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_370_special_status__compact_federalism_reading, '5c7f1f8a-554c-4686-bc50-40346aab65f4').
narrative_ontology:cs_kernel_codification('5c7f1f8a-554c-4686-bc50-40346aab65f4', formalized).
narrative_ontology:cs_authority_grounding('5c7f1f8a-554c-4686-bc50-40346aab65f4', lineage).
narrative_ontology:cs_interpretation_layer_present('5c7f1f8a-554c-4686-bc50-40346aab65f4').
narrative_ontology:cs_reading_relation('5c7f1f8a-554c-4686-bc50-40346aab65f4', article_370_special_status__abrogation_2019_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c7f1f8a-554c-4686-bc50-40346aab65f4', article_370_special_status__temporary_provision_reading, forecloses).
narrative_ontology:cs_axiom('5c7f1f8a-554c-4686-bc50-40346aab65f4', foundational, accession_compact_binding_irreversibility).
narrative_ontology:cs_axiom_status(accession_compact_binding_irreversibility, holdable).
narrative_ontology:cs_axiom_grounding('5c7f1f8a-554c-4686-bc50-40346aab65f4', accession_compact_binding_irreversibility, deontological).
narrative_ontology:cs_axiom('5c7f1f8a-554c-4686-bc50-40346aab65f4', secondary, federalism_constraint_on_central_unilateral_authority).
narrative_ontology:cs_axiom_status(federalism_constraint_on_central_unilateral_authority, holdable).
narrative_ontology:cs_axiom_grounding('5c7f1f8a-554c-4686-bc50-40346aab65f4', federalism_constraint_on_central_unilateral_authority, deontological).
narrative_ontology:cs_reference_frame('5c7f1f8a-554c-4686-bc50-40346aab65f4', compact_federalism_accession_framework).
narrative_ontology:cs_drift_state('5c7f1f8a-554c-4686-bc50-40346aab65f4', contemporary_post_2019_abrogation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5c7f1f8a-554c-4686-bc50-40346aab65f4', '').
narrative_ontology:cs_kernel_id(article_370_special_status__compact_federalism_reading, article_370_special_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_370_special_status__compact_federalism_reading, compact_federalism_theory).
narrative_ontology:constraint_beneficiary(article_370_special_status__compact_federalism_reading, jammu_kashmir_autonomous_governance).
narrative_ontology:constraint_victim(article_370_special_status__compact_federalism_reading, integrationist_constitutional_uniformity).
narrative_ontology:constraint_victim(article_370_special_status__compact_federalism_reading, central_unilateral_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTEGRATIONIST UNIFORMITY (SNARE) — The compact-federalism reading suppresses the constitutional logic of progressive integration toward uniform statehood. On this reading, the center's unilateral authority to revise the accession terms is blocked — trapped by the compact framework itself. The integrationist vision cannot exit the binding precedent without denying the reading's core premise.
constraint_indexing:constraint_classification(article_370_special_status__compact_federalism_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: UNION OF INDIA / CENTRAL AUTHORITY (TANGLED ROPE) — The compact reading both coordinates and constrains the center. It coordinates the condition of accession (the union exists only on these terms) but suppresses unilateral central revision. The center experiences moderate extraction — it retains institutional authority over the union but cannot revise the compact unilaterally. Exit is costly: abandoning the compact logic requires constitutional amendment through the constituent assembly named in the compact itself.
constraint_indexing:constraint_classification(article_370_special_status__compact_federalism_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: J&K AUTONOMOUS GOVERNANCE (ROPE) — The compact reading sustains the coordination mechanism: the accession's terms (autonomy in exchange for union) remain binding. J&K governance coordinates internal autonomy with external union membership. This reading does not present extraction from J&K's perspective — it presents the autonomy as the price J&K accepted, not a gift granted and revocable. The constraint enables coordination with residual suppression only insofar as the center retains union-level authority.
constraint_indexing:constraint_classification(article_370_special_status__compact_federalism_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: CONSTITUTIONAL FEDERALISM DOCTRINE (TANGLED ROPE) — From the institutional perspective of federal constitutional theory, the compact reading represents a binding coordination mechanism (federalism itself) that simultaneously enforces asymmetry: the union and the states exist in a structured relationship where some terms are amendable (ordinary articles) and some are not (the accession compact). The doctrine coordinates the federal structure while suppressing unilateral central override. This is the perspective that sees both the coordination function and the enforcement requirement clearly.
constraint_indexing:constraint_classification(article_370_special_status__compact_federalism_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal analytical perspective, the compact reading instantiates a logical necessity: if a union is formed by compact (stated condition of entry), then the compact is a structural foundation that cannot be unilaterally revised by one party to the compact. This appears as a natural law of federal constitutionalism — an irreducible logical constraint. The engine's false summit detector will flag whether this is genuine constitutional logic or naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(article_370_special_status__compact_federalism_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: STATUTORY IMPLEMENTATION LAYER (PITON) — The compact reading, while sustained doctrinally, is substantially performative in administrative practice. The center has operated through ordinary constitutional articles to erode J&K autonomy (through governor's rule, central legislation, bureaucratic override), while the doctrinal compact remains formally intact. The theater is the continued invocation of J&K's special status even as its functional autonomy is hollowed. This perspective sees the constraint as degraded — maintained through institutional inertia but functionally atrophied.
constraint_indexing:constraint_classification(article_370_special_status__compact_federalism_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_370_special_status__compact_federalism_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(article_370_special_status__compact_federalism_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(article_370_special_status__compact_federalism_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(article_370_special_status__compact_federalism_reading, TR),
    TR >= 0.70.

:- end_tests(article_370_special_status__compact_federalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The compact reading establishes that the center cannot unilaterally revise the accession terms, but it does not grant J&K absolute autonomy — the union retains control over defense, foreign affairs, and communications. The extractiveness reflects the partial suppression of central unilateral authority and the binding force of the accession compact itself. The value is moderate because the compact itself is a negotiated arrangement (both parties benefit from the union), not pure extraction. Suppression (0.62): Moderate-high. The compact reading suppresses the center's authority to revise unilaterally, suppresses integrationist constitutional logic, and suppresses the notion that temporary designation implies revocability. The rise in suppression requirement from 1975 to 1990 reflects the escalating administrative and enforcement measures needed to maintain central authority while the doctrinal compact remained formally intact. Theater ratio (0.48): Moderate. The constraint exhibits substantial performative content: the compact is sustained doctrinally while its functional autonomy is eroded through statutory measures (governors' rule, central legislation, bureaucratic override). The theater remains moderate rather than high because the doctrinal content is substantive — the reading is grounded in genuine constitutional principle, not merely ritual. The measurement trajectory shows stability in theater (0.48 → 0.55 during crisis → 0.48 plateau), suggesting that the performative aspect is endemic rather than degrading.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence across power positions and exit options. The integrationist perspective (powerless/trapped) sees the compact reading as a snare — it is trapped by the binding nature of the accession terms. The central authority (institutional/arbitrage) sees a tangled rope — it coordinates the union through the accession but is constrained by the compact logic. J&K autonomous governance (organized/constrained) sees pure coordination (rope) — the accession terms sustain the internal autonomy it bargained for. Federal constitutional doctrine (institutional/constrained) sees both coordination (federalism itself) and suppression (unilateral revision blocked) as the defining feature — tangled rope is the accurate classification. The analytical observer risks seeing a mountain (natural law of federalism) but the structural data suggests false summit: the binding force depends on accepting the compact theory, not on logical necessity alone. The statutory implementation layer (institutional/constrained) sees piton — the doctrine is sustained but functionally degraded, maintained through administrative theater rather than substantive autonomy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective flows from structural position: beneficiaries of the compact logic (federalism doctrine, J&K autonomous governance) derive lower d values (less extracted); victims of the suppression on the reading (integrationist constitutionalism, central unilateral authority) derive higher d values. The integrationist perspective is powerless/trapped — it has no exit from the compact framework and bears full suppression of its constitutional logic (d ≈ 0.95, near-full target). The center (institutional/arbitrage) has some exit options (can attempt constitutional amendment, can work through statutory measures) but the compact reading constrains those options (d ≈ 0.50, symmetric). Federal doctrine (institutional/constrained) sees the compact as beneficiary — the reading sustains the federal principle (d ≈ 0.25, partial beneficiary). J&K governance (organized/constrained) is ambiguous: benefits from autonomy guarantee but constrained by union membership (d ≈ 0.45, slight victim). The analytical observer (analytical/analytical) derives d ≈ 0.72 per canonical, but the natural law risk means this perspective may be identity-locked into naturalizing the reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compact_vs_temporary_designation,
    'Is Article 370''s designation as ''temporary'' (in its heading) constitutive of its meaning, or is it a nominal artifact describing the assumed duration rather than the binding power?',
    'Textual analysis of constitutional drafting debates, Constituent Assembly records on intention for ''temporary'' label, comparison with other ''temporary'' provisions and their actual duration. Historical practice: did Congress-era governments treat it as revocable, or as binding within the accession framework?',
    'If ''temporary'' is constitutive of meaning: reading is substantially weakened — any termination is legitimate, reading collapses toward temporary_provision_reading. If ''temporary'' is nominal (describing expected duration, not binding power): reading is strengthened — compact logic holds regardless of duration assumption.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compact_vs_temporary_designation, empirical, 'Whether ''temporary'' in Article 370''s heading modifies binding power or duration').

omega_variable(
    constituent_assembly_extinguishment,
    'When the Constituent Assembly dissolved in 1950, did it take the revocation power with it, or did that power devolve to the successor framework (Parliament + amendment procedures)?',
    'Constitutional law analysis of successor authority: does revocation require literal consent of the 1947 Constituent Assembly (now impossible), or can revocation occur through the amendment procedures the Constituent Assembly itself established? Compare with other accession compacts (Hyderabad, Junagadh) and their revocation procedures.',
    'If revocation requires the literal dissolved assembly: compact becomes immutable by design — reading is strongest. If power devolved to Parliament/amendment procedures: compact remains binding but revisable, reading is moderate. If reading permits unilateral presidential action without amendment: reading collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constituent_assembly_extinguishment, conceptual, 'Authority transfer after Constituent Assembly dissolution').

omega_variable(
    accession_instruments_binding_force,
    'Do the Instruments of Accession (signed 1947) have the legal force of international treaty, domestic contract, constitutional compact, or something hybrid? Does the category determine whether unilateral termination is constitutionally possible?',
    'Comparative study of accession instruments vs treaties vs constitutional amendments. Analysis of whether international precedent (treaty termination requires bilateral consent vs unilateral exit clauses) informs constitutional interpretation. Legal doctrine on whether founding contracts can be unilaterally revised by one party.',
    'If treaty-like: unilateral termination is prohibited — reading is strong. If domestic contract: depends on contract terms and dissolution clauses. If constitutional compact: binding unless amendment procedures explicitly permit unilateral termination — reading is strong. If hybrid with treaty properties: strongest precedent protections.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accession_instruments_binding_force, conceptual, 'Legal category of Instruments of Accession and binding force').

omega_variable(
    reading_naturalization_risk,
    'Does this reading naturalize the 1947 accession process (treating it as a timeless constitutional foundation) rather than recognizing it as a contingent historical act that subsequent parties can in principle revisit?',
    'Philosophical analysis: is the reading committed to the idea that accessions are irreversible by definition, or that THIS accession was specified as irreversible? Historical counterfactual: what would the reading predict if a later state sought to un-accede or renegotiate terms?',
    'If naturalization (accessions are inherently binding): reading is rigid, applicable to any accession regardless of context. If specific to the 1947 Jammu & Kashmir accession: reading is contingent, permits other readings for other accessions. The naturalization risk is the mountain perspective''s potential false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_naturalization_risk, conceptual, 'Whether the reading naturalizes accession irreversibility or grounds it in specific historical commitment').

omega_variable(
    federal_bargaining_power_asymmetry,
    'If the compact reading is correct, does J&K''s special status represent J&K''s bargaining power in 1947, or does it represent a structural constraint the center accepted to secure accession? Can the center renegotiate the terms now without J&K''s consent?',
    'Historical analysis of 1947 accession negotiations: what alternatives did J&K have? What would have happened if J&K refused to accede? Was J&K in a position to demand autonomy, or was autonomy imposed by the center to achieve accession? Comparison with other accessions.',
    'If J&K had genuine bargaining power: compact represents negotiated equality, strengthens the reading. If autonomy was center''s offer to secure accession: still binding, but framed differently — center cannot revoke its own offer unilaterally. Either way, reading is strong. If J&K had no alternatives: reading''s moral foundation weakens, though legal binding force may persist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_bargaining_power_asymmetry, empirical, '1947 accession: J&K''s bargaining power and autonomy terms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_370_special_status__compact_federalism_reading, 1947, 2019).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(article_370_theater_1947_accession, article_370_special_status__compact_federalism_reading, theater_ratio, 1947, 0.25).
narrative_ontology:measurement(article_370_theater_1975_emergency, article_370_special_status__compact_federalism_reading, theater_ratio, 1975, 0.48).
narrative_ontology:measurement(article_370_theater_1990_insurgency, article_370_special_status__compact_federalism_reading, theater_ratio, 1990, 0.55).
narrative_ontology:measurement(article_370_theater_2010_plateau, article_370_special_status__compact_federalism_reading, theater_ratio, 2010, 0.48).

% Extraction over time
narrative_ontology:measurement(article_370_extractiveness_1947_accession, article_370_special_status__compact_federalism_reading, base_extractiveness, 1947, 0.15).
narrative_ontology:measurement(article_370_extractiveness_1975_emergency, article_370_special_status__compact_federalism_reading, base_extractiveness, 1975, 0.32).
narrative_ontology:measurement(article_370_extractiveness_1990_insurgency, article_370_special_status__compact_federalism_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(article_370_extractiveness_2010_plateau, article_370_special_status__compact_federalism_reading, base_extractiveness, 2010, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(article_370_suppression_1947_accession, article_370_special_status__compact_federalism_reading, suppression_requirement, 1947, 0.35).
narrative_ontology:measurement(article_370_suppression_1975_emergency, article_370_special_status__compact_federalism_reading, suppression_requirement, 1975, 0.62).
narrative_ontology:measurement(article_370_suppression_1990_insurgency, article_370_special_status__compact_federalism_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(article_370_suppression_2010_plateau, article_370_special_status__compact_federalism_reading, suppression_requirement, 2010, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_370_special_status__compact_federalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_370_special_status__compact_federalism_reading, article_370_special_status__abrogation_2019_reading).
narrative_ontology:affects_constraint(article_370_special_status__compact_federalism_reading, article_370_special_status__temporary_provision_reading).

% DUAL FORMULATION NOTE:
% Article 370 special status constraint family: three readings of the same kernel article_370_special_status, three structurally distinct claims with different ε values, different suppression profiles, and different beneficiary/victim structures. Each reading is a separate constraint story; the network edges record the family relationship and the doctrinal contest. The compact-federalism reading (this story) suppresses unilateral central revision and enforces the binding nature of the accession compact. The temporary_provision_reading treats the 'temporary' heading as constitutive and thus interprets the provision as always destined for termination. The abrogation_2019_reading treats the 2019 abrogation as a valid constitutional act, testing the framework on itself. The three readings have different ε values because they instantiate different structural facts about who can bind whom and how revision is possible. The compact-federalism reading argues for binding force through the accession mechanism; the temporary reading argues for built-in termination; the abrogation reading describes the termination that occurred. They are not three perspectives on one constraint; they are three constraints, each grounded in a different reading of the contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_370_special_status__compact_federalism_reading, institutional, 0.5).
constraint_indexing:directionality_override(article_370_special_status__compact_federalism_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
