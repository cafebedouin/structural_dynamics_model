% ============================================================================
% CONSTRAINT STORY: individual_right_reading__heller_core_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_individual_right_reading__heller_core_reading, []).

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
 *   constraint_id: individual_right_reading__heller_core_reading
 *   human_readable: Heller Core Reading: Individual Right to Handguns in the Home
 *   domain: constitutional_law/second_amendment
 *
 * SUMMARY:
 *   District of Columbia v. Heller (2008) fixed the Second Amendment as
 *   protecting an individual right to possess handguns in the home for
 *   self-defense, with a holding that longstanding regulations remain
 *   presumptively valid. This is ONE reading of a contested kernel: the
 *   individual-right-in-handgun claim. The Heller reading establishes the
 *   core (home handguns) as protected while preserving the periphery
 *   (licensing, registration, carry restrictions, felon disqualifications) as
 *   constitutional territory. The constraint operates as a bifurcation:
 *   beneficiaries are armed householders and the self-defense constituency;
 *   victims are categorical prohibition regimes (jurisdictions with blanket
 *   handgun bans). The extractiveness emerges not from the right itself but
 *   from the suppression it imposes on contrary regulatory choices. The
 *   reading exhibits Tangled Rope structure: it solves the coordination
 *   problem of a nationally fragmented right (pure rope function) while
 *   simultaneously extracting from prohibition jurisdictions by foreclosing
 *   their core regulatory authority (snare function). The theater ratio has
 *   increased over the interval as the 'fixed' core requires sustained
 *   judicial restatement and reconfirmation, particularly through post-Heller
 *   litigation testing the core-periphery boundary and post-Bruen litigation
 *   reconciling Heller's presumption with Bruen's methodology.
 *
 * KEY AGENTS:
 *   - Armed Householders / Self-Defense Constituency: Primary beneficiary (institutional/arbitrage) — gain recognized constitutional right with low ongoing suppression; can relocate or litigate; arbitrage available
 *   - Categorical Prohibition Regimes (Cities & States): Primary victim (powerless/trapped) — must surrender handgun bans or accept constitutional non-compliance; no arbitrage; trapped in legal conflict with supreme authority
 *   - Peripheral Regulation Constituencies (Gun Violence Prevention): Secondary victim (moderate/constrained) — benefit from clarified legal boundaries but suffer suppression in regulation of features, licensing, carrying; constrained exit because core is fixed
 *   - Conservative / Originalist Jurisprudence: Institutional maintainer (institutional/arbitrage) — sustains the originalist method through litigation and doctrinal theater; benefits from method's perceived legitimacy
 *   - Pro-Regulation Organized Actors: Democratic alternative (organized/constrained) — see amendment pathway as structural sunset; constrained because core persists until amendment
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the individual-right reading as immutable principle rather than contingent doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(individual_right_reading__heller_core_reading, 0.38).
domain_priors:suppression_score(individual_right_reading__heller_core_reading, 0.52).
domain_priors:theater_ratio(individual_right_reading__heller_core_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(individual_right_reading__heller_core_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(individual_right_reading__heller_core_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(individual_right_reading__heller_core_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(individual_right_reading__heller_core_reading, tangled_rope).
narrative_ontology:human_readable(individual_right_reading__heller_core_reading, "Heller Core Reading: Individual Right to Handguns in the Home").
narrative_ontology:topic_domain(individual_right_reading__heller_core_reading, "constitutional_law/second_amendment").

domain_priors:requires_active_enforcement(individual_right_reading__heller_core_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(individual_right_reading__heller_core_reading, '51aa9bdb-d9c7-42f1-b2ce-910643e00165').
narrative_ontology:cs_kernel_codification('51aa9bdb-d9c7-42f1-b2ce-910643e00165', formalized).
narrative_ontology:cs_authority_grounding('51aa9bdb-d9c7-42f1-b2ce-910643e00165', lineage).
narrative_ontology:cs_interpretation_layer_present('51aa9bdb-d9c7-42f1-b2ce-910643e00165').
narrative_ontology:cs_reading_relation('51aa9bdb-d9c7-42f1-b2ce-910643e00165', individual_right_reading__bruen_methodology_reading, influences).
narrative_ontology:cs_reading_relation('51aa9bdb-d9c7-42f1-b2ce-910643e00165', individual_right_reading__sensitive_places_reading, influences).
narrative_ontology:cs_axiom('51aa9bdb-d9c7-42f1-b2ce-910643e00165', foundational, core_home_handgun_protected).
narrative_ontology:cs_axiom_status(core_home_handgun_protected, holdable).
narrative_ontology:cs_axiom_grounding('51aa9bdb-d9c7-42f1-b2ce-910643e00165', core_home_handgun_protected, deontological).
narrative_ontology:cs_axiom('51aa9bdb-d9c7-42f1-b2ce-910643e00165', foundational, longstanding_regulations_presumptively_valid).
narrative_ontology:cs_axiom_status(longstanding_regulations_presumptively_valid, holdable).
narrative_ontology:cs_axiom_grounding('51aa9bdb-d9c7-42f1-b2ce-910643e00165', longstanding_regulations_presumptively_valid, empirically_contingent).
narrative_ontology:cs_reference_frame('51aa9bdb-d9c7-42f1-b2ce-910643e00165', individual_right_constrained_by_historical_practice).
narrative_ontology:cs_drift_state('51aa9bdb-d9c7-42f1-b2ce-910643e00165', post_bruen_2022_present, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('51aa9bdb-d9c7-42f1-b2ce-910643e00165', '').
narrative_ontology:cs_kernel_id(individual_right_reading__heller_core_reading, individual_right_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(individual_right_reading__heller_core_reading, armed_householders).
narrative_ontology:constraint_beneficiary(individual_right_reading__heller_core_reading, self_defense_constituency).
narrative_ontology:constraint_victim(individual_right_reading__heller_core_reading, categorical_prohibition_regimes).
narrative_ontology:constraint_victim(individual_right_reading__heller_core_reading, localities_with_handgun_bans).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CATEGORICAL PROHIBITION REGIMES (SNARE) — Cities and states with longstanding handgun bans face a structural trap: Heller's holding suppresses their regulatory authority in the core domain (home defense) while offering no compensating benefit. These jurisdictions must either surrender the ban or accept constitutional non-compliance. They cannot exit or renegotiate; the constraint extracts compliance through legal coercion. Maximum experienced suppression because alternatives have been foreclosed by the supreme authority's reading.
constraint_indexing:constraint_classification(individual_right_reading__heller_core_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ARMED HOUSEHOLDERS (ROPE) — Experience the constraint as pure coordination: Heller establishes a shared framework for home self-defense rights. The constraint solves a collective action problem — without judicial resolution, the right to armed self-defense would remain contested across 50 jurisdictions. Beneficiaries have arbitrage options (relocate to permissive jurisdictions, or litigate) and benefit from coordination. Low effective extraction; genuine coordination function.
constraint_indexing:constraint_classification(individual_right_reading__heller_core_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PERIPHERAL REGULATION CONSTITUENCIES (TANGLED ROPE) — Gun violence prevention advocates, public health researchers, and moderate-gun-regulation constituencies face a hybrid structure. They benefit from the coordinate framework Heller establishes (no longer litigating the core; the core is off the table). But they suffer extraction in the periphery — their capacity to regulate features, licensing, registration, and carrying is suppressed by the reading's implication that 'longstanding' regulations are presumptively valid while new regulations face strict scrutiny. Mixed coordination and asymmetric extraction. Constrained exit because they cannot renegotiate the core without reversing Heller.
constraint_indexing:constraint_classification(individual_right_reading__heller_core_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSERVATIVE JURISPRUDENCE / ORIGINALIST TRADITION (PITON) — From the perspective of conservative constitutional scholars and judges, the reading is largely performative: it claims to 'fix' the core as immutable while actually performing an act of constitutional interpretation that requires ongoing maintenance, re-reading of historical texts, and restatement of what 'original public meaning' was. The originalist method is described as constraining judicial discretion, but the reading sustains the method through doctrinal theater — repeated invocation of founding-era sources without mechanistic constraint. High theater ratio because the 'fixed' core requires constant restatement and reconfirmation through legal argumentation.
constraint_indexing:constraint_classification(individual_right_reading__heller_core_reading, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PRO-REGULATION ORGANIZED ACTORS / DEMOCRATIC INTERVENTION (SCAFFOLD) — Gun violence prevention organizations and democratic coalitions see the constraint as temporary, with a structural sunset: constitutional amendment. The 28th Amendment pathway (overturning or limiting Heller) remains theoretically open, though requiring supermajority action. This perspective treats the constraint as coordination with an enforced sunset clause — the core is protected until democratic will achieves amendment. Extractiveness is low from this view because a formal exit mechanism (amendment) exists, and the constraint is understood as constraining *both* sides until amendment or sustained political change.
constraint_indexing:constraint_classification(individual_right_reading__heller_core_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW FRAME (MOUNTAIN) — From a civilizational/universal view, an individual right to armed self-defense appears as an unchangeable axiom grounded in natural law — the right to preserve one's life and protect one's home is prior to government and cannot be alienated by legislation. Longstanding regulations are presumptively untouched not as a doctrine but as recognition that regulations too have constitutional legitimacy rooted in historical practice. Both sides (the right and historic regulation) appear as immutable principles in equilibrium. However, the structural data contradicts this — identifiable beneficiaries exist, suppression is active, and the constraint requires legal enforcement. The engine's false summit detector will flag this as naturalization.
constraint_indexing:constraint_classification(individual_right_reading__heller_core_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: PROHIBITION JURISDICTIONS / INSTITUTIONAL ACTOR (TANGLED ROPE) — Municipalities and states with handgun bans (e.g., Washington DC pre-Heller) occupy a constrained institutional position. They benefit from Heller's coordination framework (no longer living under uncertainty about the constitutional right's status) but suffer extraction through suppression of their core regulatory authority. They cannot arbitrage — they must comply or litigate further. This differs from the powerless perspective because these institutions have secondary strategies (regulation of periphery, enforcement mechanisms, licensing requirements) that allow some adaptation. Constrained exit, mixed beneficiary/victim status.
constraint_indexing:constraint_classification(individual_right_reading__heller_core_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(individual_right_reading__heller_core_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(individual_right_reading__heller_core_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(individual_right_reading__heller_core_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(individual_right_reading__heller_core_reading, TR),
    TR >= 0.70.

:- end_tests(individual_right_reading__heller_core_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The core constraint — suppression of categorical handgun bans — extracts compliance from prohibition jurisdictions. But extraction is not severe because the suppression is targeted (core only; periphery presumptively valid) and because alternative regulatory pathways remain available (licensing, background checks, felon disqualifications, sensitive places). Over the interval (2008–2024), extractiveness has increased from 0.28 to 0.38 as post-Heller litigation has gradually expanded the core (shall-issue carry pushed handguns outside the home into protected status; Bruen methodology challenged historical regulations' presumptive validity). Suppression (0.52): Moderate-high. Prohibition jurisdictions face substantial suppressive force — legal invalidity of their chosen policy — but suppression is not total because peripheral regulations remain available. Theater ratio (0.48): Moderate. Heller's holding claimed to 'fix' the core, but subsequent litigation has exposed the core-periphery boundary as contestable. The 'fixed' position requires continuous judicial restatement and reconfirmation. Theater has increased from 0.35 to 0.48 as conservative jurisprudence defends the originalist method against criticism that it is not mechanistically constraining.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits sharp perspectival divergence. For armed householders, it is pure coordination (Rope) — solving a legitimacy problem. For prohibition cities, it is pure extraction (Snare) — their chosen policy is invalidated without compensation. For gun-violence-prevention constituencies, it is hybrid (Tangled Rope) — clarified boundaries help them coordinate internally, but the protected core suppresses their peripheral regulatory options. For originalist jurisprudence, it is mostly theater (Piton) — the method claims to constrain interpretation, but interpreting 'original public meaning' requires substantial discretion. For democratic amendment constituencies, it is a temporary constraint with sunset (Scaffold) — the 28th Amendment pathway offers formal exit. For the civilizational analytical observer, it risks appearing as natural law (Mountain) — an immutable principle. The gap between the beneficiary's rope classification and the victim's snare classification reveals the constraint's hybrid structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each agent is computed from power level, exit options, and structural relationship. Armed householders (institutional/arbitrage) experience low effective extraction (d ≈ 0.15 → f(d) ≈ -0.01) because they are beneficiaries with exit options. Prohibition jurisdictions (powerless/trapped) experience high effective extraction (d ≈ 0.95 → f(d) ≈ 1.42) because they are victims with no exit — they cannot opt out of the constitutional reading. Moderate gun-violence-prevention constituencies (moderate/constrained) experience moderate extraction (d ≈ 0.72 → f(d) ≈ 1.15) because they are secondary victims with constrained but non-zero exit options (regulation of periphery, advocacy for amendment). The scope modifier (national) applies to all perspectives, dampening slightly but not eliminating the directionality effect. The key structural difference: beneficiaries have arbitrage; victims have none.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by establishing that the individual-right-in-handgun claim is a genuinely hybrid constraint: it coordinates (solves the fragmentation problem) and extracts (suppresses prohibition regimes) simultaneously. Heller is NOT pure coordination (Rope) because identifiable victims exist and suppression is active. Heller is NOT pure extraction (Snare) because genuine coordination gains exist (uncertainty resolved, legal clarity established) and peripheral regulations remain available. The classification as Tangled Rope captures this hybrid structure exactly. The false-summit perspective (mountain classification) serves as a diagnostic check: if the reading were genuinely immutable natural law, it would show no beneficiaries and no suppression; the presence of both reveals contingency. The mandatrophy is resolved by accepting the perspectival plurality — the constraint is legitimately rope to beneficiaries, snare to victims, scaffold to amendment constituencies, and piton to jurisprudence that must sustain the method.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historicity_of_longstanding_regulations,
    'Are longstanding regulations (pre-dating 1868 or pre-dating the founding) genuinely immutable constitutional law, or does ''presumptively untouched'' embed a rebuttable doctrine that allows historical regulations to be struck down on equal protection or public health grounds?',
    'Jurisprudential evolution through post-Heller cases testing whether historical regulations survive strict scrutiny when applied to modern contexts (e.g., historical felon disqualifications applied to non-violent offenders, historical ''surety'' requirements applied to modern threat contexts). The operative question: can any historical regulation be struck down, or are some truly immutable?',
    'If immutable: Heller establishes a bifurcated constitutional order with a protected core and an untouchable periphery — the core-periphery boundary becomes the site of all regulatory contestation. If rebuttable: ''presumptively untouched'' collapses into strict-scrutiny analysis, and the core-periphery distinction becomes nominal rather than structural. Classification would shift from Tangled Rope toward Piton (the presumption is theater if it can be rebutted).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historicity_of_longstanding_regulations, empirical, 'Whether longstanding regulations are truly immutable or rebuttably presumed valid').

omega_variable(
    core_periphery_boundary_instability,
    'Where is the boundary between the core (home handguns for self-defense) and the periphery (features, licensing, registration, carrying outside the home)? Does the boundary move as constitutional doctrine develops, or is it fixed by Heller''s holding?',
    'Analysis of post-Heller litigation distinguishing denied and granted petitions: cases where courts treat a restriction as core vs peripheral. Look for patterns in how circuit courts apply Heller''s two-step framework. Identify whether the core expands (e.g., shall-issue carry laws push handguns outside the home into the core) or contracts (e.g., felon disqualifications become peripheral).',
    'If boundary is fixed: the core-periphery distinction is stable law, and Heller''s constraint type remains Tangled Rope with consistent structure. If boundary moves: the constraint exhibits ''creep'' — beneficiaries push to expand the core, prohibition jurisdictions push to contract it. Expanding core → higher extractiveness; contracting core → lower extractiveness. Classification would shift toward Piton (boundaries sustained by litigation theater rather than principle).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(core_periphery_boundary_instability, empirical, 'Stability and scope of the core-periphery boundary').

omega_variable(
    bruen_versus_heller_reconciliation,
    'How does Bruen''s methodology (no interest balancing, requires historical analogue) reconcile with Heller''s presumptively-untouched-longstanding-regulations language? Can a regulation be both longstanding-and-thus-presumptively-valid AND lacking-a-historical-analogue-and-thus-struck-down?',
    'Doctrinal analysis of post-Bruen cases testing historical regulations that lack founding-era analogues (e.g., modern felon-in-possession laws, modern domestic violence disqualifications). Determine whether courts treat ''presumptively untouched'' as overriding Bruen''s historical requirement or subordinate to it.',
    'If Bruen dominates: ''presumptively untouched'' becomes narrower — only regulations with founding-era analogues survive. Extractiveness in the periphery decreases (fewer regulations are protected). If Heller presumption dominates: Bruen''s strict methodology is tempered by the presumption. The constraint''s bifurcation (core protected, longstanding peripheral untouched) becomes robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bruen_versus_heller_reconciliation, empirical, 'Reconciliation of Heller presumption with Bruen methodology').

omega_variable(
    kernel_reading_committer_divergence,
    'This constraint instantiates ONE reading of the individual-right-in-handgun kernel. The sibling Bruen reading claims no interest-balancing methodology; the sensitive-places reading claims a geographic exception. Do these readings coexist within the same constitutional framework, or does Heller''s ''core fixed'' axiom foreclose one or both siblings?',
    'Doctrinal analysis of how the three readings interact in actual litigation. Do courts cite Heller, Bruen, and sensitive-places language simultaneously without contradiction? Or do they choose among the readings, treating some as latent or abandoned?',
    'If coexist: the kernel exhibits genuine ambiguity; all three readings are live positions held by different parties. Classification remains Tangled Rope (coordination with embedded extraction). If Heller forecloses Bruen: the readings cannot coexist; Bruen would be a failed attempt to overturn Heller''s core. If Bruen forecloses Heller: Heller''s presumption is subordinated to method, and this reading''s stability erodes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_divergence, conceptual, 'Whether sibling readings coexist or whether one reading logically forecloses another').

omega_variable(
    false_summit_diagnostic,
    'Is Heller''s core (handguns in the home for self-defense) a genuinely immutable constitutional principle grounded in natural law (Mountain), or is it a contingent institutional reading that naturalizes the individual-right doctrine to suppress prohibition regimes (false summit — should be reclassified to Tangled Rope)?',
    'Comparative constitutional analysis: examine how other jurisdictions (UK, Canada, Germany, Australia) constitute the self-defense right without Heller''s individual-right framework. If robust, socially functional self-defense regulation exists without recognizing an individual right to home handguns, the immutability claim is undermined. Alternatively, examine whether the individual-right reading emerged contingently from particular institutional interests (NRA litigation strategy starting in the 1970s) rather than from immutable constitutional text.',
    'If Mountain (immutable): Heller is binding law from which only constitutional amendment can depart. If Tangled Rope with false-summit dynamics: the reading naturalizes what is actually a contingent institutional choice; reclassification would flag the constraint as extractive (suppressing prohibition regimes through naturalization) rather than coordinate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_diagnostic, conceptual, 'Whether the core is immutable principle or contingent naturalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(individual_right_reading__heller_core_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(heller_core_theater_t0, individual_right_reading__heller_core_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(heller_core_theater_t8, individual_right_reading__heller_core_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement(heller_core_theater_t16, individual_right_reading__heller_core_reading, theater_ratio, 16, 0.48).

% Extraction over time
narrative_ontology:measurement(heller_core_extract_t0, individual_right_reading__heller_core_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(heller_core_extract_t8, individual_right_reading__heller_core_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(heller_core_extract_t16, individual_right_reading__heller_core_reading, base_extractiveness, 16, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(individual_right_reading__heller_core_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(individual_right_reading__heller_core_reading, individual_right_reading__bruen_methodology_reading).
narrative_ontology:affects_constraint(individual_right_reading__heller_core_reading, individual_right_reading__sensitive_places_reading).

% DUAL FORMULATION NOTE:
% The individual-right-in-handgun kernel exhibits three structurally distinct readings with different extractiveness and classification profiles. This story instantiates the Heller core reading (ε ≈ 0.38, Tangled Rope). The Bruen methodology reading (ε ≈ 0.45, Tangled Rope with stricter standards) and the sensitive-places reading (ε ≈ 0.52, Snare for prohibition-by-geography jurisdictions) are separate constraint stories linked via network.affects_constraints. The readings coexist in post-2022 constitutional doctrine but with increasing tension as Bruen's methodology tests the Heller presumption's scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(individual_right_reading__heller_core_reading, institutional, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
