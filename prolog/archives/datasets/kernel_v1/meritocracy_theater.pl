% ============================================================================
% CONSTRAINT STORY: meritocracy_theater
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_meritocracy_theater, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: meritocracy_theater
 *   human_readable: Meritocracy Theater: Performative Selection Masking Structural Extraction
 *   domain: social/institutional/economic
 *
 * SUMMARY:
 *   Meritocracy theater is a structural constraint where selection
 *   institutions use a performative narrative of objective merit-based
 *   selection to legitimize inequality while extracting effort, cost, and
 *   compliance from structurally excluded populations. The constraint
 *   operates by shifting causal attribution from institutional structure to
 *   individual capability: if you did not advance, you did not try hard
 *   enough or lack sufficient merit. This reframing prevents excluded
 *   populations from recognizing shared structural barriers and organizing
 *   collective response. The constraint exhibits characteristics of both pure
 *   extraction (snare) and false natural law (mountain): it performs as if
 *   merit-based selection is a neutral, objective, inevitable institutional
 *   necessity when in fact merit definitions encode incumbent advantage,
 *   selection mechanisms correlate with privilege rather than capability, and
 *   the entire system serves to concentrate opportunity while distributing
 *   blame. The measurement trajectory shows theater_ratio increasing from
 *   0.68 to 0.81 over the interval, indicating that selection institutions
 *   have become more elaborate in their legitimacy performance (expanded
 *   holistic review procedures, more sophisticated ranking methodologies,
 *   increased rhetorical emphasis on diversity) precisely as material
 *   barriers to access have intensified and post-credential disparities have
 *   grown. This inverse relationship — more performative sophistication as
 *   functional barriers increase — is diagnostic of piton dynamics and snare
 *   suppression.
 *
 * KEY AGENTS:
 *   - Structurally Excluded Aspirants: Primary victims (powerless/trapped) — bear extraction of effort, cost, and internalized blame; cannot perceive shared barriers due to suppression mechanism
 *   - Incumbent Privileged Cohorts: Primary beneficiaries (institutional/arbitrage or powerful/mobile) — capture opportunity concentration and credential scarcity value; have awareness of structural advantage
 *   - Credential Gatekeepers (Universities, Professional Licensing Bodies): Secondary beneficiaries (institutional/arbitrage) — extract tuition, testing fees, credential scarcity rent; maintain gatekeeping through meritocracy legitimacy
 *   - Selection Institution Administrators: Institutional actors (institutional/arbitrage) — experience constraint as coordination mechanism; benefit from legitimacy and resource autonomy
 *   - Reform-Minded Admissions Officers: Institutional actors (institutional/constrained) — see theatrical nature of selection machinery but have constrained exit options (career path dependence on institutional legitimacy)
 *   - Marginal Aspirants (Partial Access): Secondary victims (moderate/constrained) — have partial access to selection machinery but face high barriers; experience both coordination function and substantial extraction
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent social necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(meritocracy_theater, 0.62).
domain_priors:suppression_score(meritocracy_theater, 0.68).
domain_priors:theater_ratio(meritocracy_theater, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(meritocracy_theater, extractiveness, 0.62).
narrative_ontology:constraint_metric(meritocracy_theater, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(meritocracy_theater, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(meritocracy_theater, snare).
narrative_ontology:human_readable(meritocracy_theater, "Meritocracy Theater: Performative Selection Masking Structural Extraction").
narrative_ontology:topic_domain(meritocracy_theater, "social/institutional/economic").

domain_priors:requires_active_enforcement(meritocracy_theater).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(meritocracy_theater, '9039a6a9-1a91-4c67-b1a9-8ccde535af53').
narrative_ontology:cs_kernel_codification('9039a6a9-1a91-4c67-b1a9-8ccde535af53', distributed).
narrative_ontology:cs_authority_grounding('9039a6a9-1a91-4c67-b1a9-8ccde535af53', extraction).
narrative_ontology:cs_interpretation_layer_present('9039a6a9-1a91-4c67-b1a9-8ccde535af53').
narrative_ontology:cs_created_at('9039a6a9-1a91-4c67-b1a9-8ccde535af53', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(meritocracy_theater, incumbent_privileged_cohorts).
narrative_ontology:constraint_beneficiary(meritocracy_theater, credential_gatekeepers).
narrative_ontology:constraint_beneficiary(meritocracy_theater, selection_institution_administrators).
narrative_ontology:constraint_victim(meritocracy_theater, structurally_excluded_populations).
narrative_ontology:constraint_victim(meritocracy_theater, effort_extraction_targets).
narrative_ontology:constraint_victim(meritocracy_theater, aspirational_non_completers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STRUCTURALLY EXCLUDED ASPIRANT (SNARE) — Trapped by lack of institutional access, generational capital, and epistemic closure about structural barriers. The meritocracy narrative assigns failure to individual inadequacy, preventing coalition formation around shared structural constraints. Maximum extraction: effort expenditure, psychological cost of internalized blame, and normalization of inequality. No exit option exists because the constraint's suppression mechanism is internalized — the aspirant believes the barrier is their own lack of merit.
constraint_indexing:constraint_classification(meritocracy_theater, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SELECTION INSTITUTION ADMINISTRATOR (ROPE) — Experiences the meritocracy narrative as a coordination mechanism: objective standards enable transparent selection, predictable outcomes, and institutional legitimacy. The administrator benefits from the constraint through budgetary autonomy, reputation stability, and reduced pressure to examine structural access barriers. Extraction runs toward this agent; they have arbitrage options (can exit selection work, can shift to other institutions). The coordination function is genuine from their vantage — the constraint does coordinate selection activity.
constraint_indexing:constraint_classification(meritocracy_theater, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: REFORM-MINDED ADMISSIONS OFFICER (PITON) — Sees the meritocracy machinery as largely theatrical: standardized test scores correlate with family wealth, not aptitude; 'cultural fit' coding masks homophily; holistic review consumes resources but produces ritual justification for predetermined outcomes. This agent has constrained exit options (career path dependence on institutional legitimacy, risk of professional retaliation for explicit critique). The theater_ratio is high because the selection process performs legitimacy while actual allocation follows prior privilege. The constraint persists through institutional inertia — the meritocracy ritual is maintained because it serves incumbent interests, not because it functions.
constraint_indexing:constraint_classification(meritocracy_theater, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PRIVILEGED BENEFICIARY—REFLEXIVE (TANGLED ROPE) — Agent from an incumbent cohort with sufficient reflexive awareness to see the constraint's hybrid nature: genuine coordination exists (transparent standards enable institutional function), but extraction runs overwhelmingly toward their cohort. Meritocratic selection produces real career outcomes for them while extracting effort and compliance from those without generational capital. This perspective has mobile exit options (can change institutions, can critique the system with lower professional risk). Extraction is not maximal because the agent has agency and sees the structure clearly. The constraint provides both real coordination and asymmetric extraction.
constraint_indexing:constraint_classification(meritocracy_theater, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: MARGINAL ASPIRANT—CREDENTIAL-SEEKING (TANGLED ROPE) — Agent from structurally constrained background who has partial access to selection machinery (can apply, can compete, can sometimes advance) but faces high barriers. The constraint delivers both genuine coordination (credentials do enable some mobility) and substantial extraction (effort expenditure far exceeds probability of success, accumulated debt from credential pursuit, internalized belief that remaining barriers are personal inadequacy). Exit options are constrained by sunk investment in the credential pathway and limited awareness of structural exclusion. The perspectival gap between this agent and the powerless aspirant (Perspective 1) reflects partial access — some participation in the meritocratic machinery, but low success probability.
constraint_indexing:constraint_classification(meritocracy_theater, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER—NATURAL LAW FRAME (MOUNTAIN) — From a civilizational universal perspective, selection mechanisms are inherent to any complex society: some differentiation is necessary, some measurement is required, and some aspirants will exceed capacity. The meritocracy frame sees this as natural law: inequality is inevitable; selection by measured ability is the fairest mechanism available; those who fail to advance lack sufficient merit. This perspective naturalizes what is actually a contingent institutional arrangement. The engine's false summit detector will identify this as a false summit: beneficiaries exist (incumbent cohorts, credential gatekeepers), indicating the natural-law framing serves extractive interests.
constraint_indexing:constraint_classification(meritocracy_theater, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meritocracy_theater_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(meritocracy_theater, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meritocracy_theater, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(meritocracy_theater, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(meritocracy_theater, TR),
    TR >= 0.70.

:- end_tests(meritocracy_theater_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderately high. The meritocracy frame legitimizes resource extraction (tuition, credential costs, opportunity concentration) by attributing outcomes to individual merit rather than structural access. The extraction is not maximal (ε ≥ 0.66 for pure snare) because the constraint does deliver some genuine coordination function — selection mechanisms do sort people into roles, do provide career pathways for some, and do enable institutional function. But the extraction is substantial because the beneficiary population captures disproportionate opportunity while the cost burden (effort, debt, psychological internalization of blame) is borne by excluded populations. The increasing trajectory (0.48 → 0.62) reflects that selection institutions have intensified both their functional filtering and their performative legitimacy over the measurement interval. Suppression (0.68): High, reflecting multiple mechanisms: structural barriers to access (cost, prior education gaps, social capital deficit), institutional opacity (lack of clarity about selection criteria and decision-making processes), and most critically, the internalized belief by excluded populations that barriers are personal inadequacy rather than structural design. The suppression_requirement rising from 0.62 to 0.68 indicates that as material access barriers have become more documented (in research and public discourse), institutions have needed to invest more in performative legitimacy to maintain the meritocracy narrative and prevent coalition formation around structural exclusion. Theater ratio (0.81): Very high. Selection procedures are substantially performative: standardized testing produces ritualized scores that predict family wealth more accurately than academic capability; holistic review consumes significant institutional resources to produce predetermined diversity optics; institutional rankings amplify credential scarcity while contributing little to institutional or educational function; aspiration narratives ('anyone can make it') are broadcast despite stable intergenerational transmission of advantage. The theater has increased from 0.68 to 0.81, indicating that selection institutions have become more elaborately performative precisely as functional outcomes have become more obviously stratified. This is the signature pattern of piton dynamics: institutional inertia, degraded function, and performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The structurally excluded aspirant (powerless/trapped) experiences pure extraction (snare) — the meritocracy narrative assigns failure to personal inadequacy, preventing recognition of shared barriers. The selection institution administrator (institutional/arbitrage) experiences coordination (rope) — transparent standards enable predictable selection and institutional legitimacy. The reform-minded admissions officer (institutional/constrained) experiences degradation (piton) — the selection machinery is largely theatrical, maintained through inertia and self-interest rather than function. The privileged beneficiary with reflexive awareness (powerful/mobile) experiences hybrid coordination-extraction (tangled_rope) — genuine career outcomes for their cohort alongside asymmetric extraction from others. The marginal aspirant with partial access (moderate/constrained) experiences asymmetric coordination-extraction (tangled_rope) — some credential mobility possible but at very high effort cost with low success probability. The civilizational analytical observer risks seeing natural law (mountain) — selection mechanisms are inherent to complex societies and meritocracy is the fairest available method. The perspectival gap is both a feature of this constraint and a diagnostic signal of its extractive nature: the target (excluded aspirants) perceives the constraint very differently from the beneficiary (privileged cohorts), which is possible precisely because the suppression mechanism prevents the target from recognizing the structural coordination benefits that accrue to the beneficiary.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) encodes each agent's structural position in the extraction flow. Structurally excluded aspirants appear as victims with trapped exit — they bear extraction and cannot easily escape; d ≈ 0.90+. Incumbent privileged cohorts appear as beneficiaries with arbitrage or mobile exit — they benefit and can shift context; d ≈ 0.1–0.2. Selection institution administrators appear as beneficiaries with arbitrage exit — they extract legitimacy and institutional autonomy; d ≈ 0.05–0.15. Reform-minded admissions officers have constrained exit (career path dependence) and awareness of extraction; d ≈ 0.55–0.65 (asymmetric victim-beneficiary position — they see the extraction but benefit from institutional standing). Marginal aspirants have constrained exit and partial victim status; d ≈ 0.70–0.75. The privileged reflexive beneficiary has mobile exit and beneficiary status but also awareness; d ≈ 0.3–0.4 (lower than true targets because of exit mobility and beneficiary position, higher than oblivious beneficiaries because of awareness of structural asymmetry). The directionality spread — from 0.05 (institutional beneficiary) to 0.92 (powerless trapped target) — generates the large perspectival gap. When f(d) is applied, beneficiaries experience effective extraction χ close to zero or negative (they are subsidized), while targets experience χ approaching 1.0 (maximum experienced extraction relative to capacity). This is the defining signature of snare classification: beneficiaries perceive coordination, targets perceive pure extraction, and the gap is maintained by suppression of structural visibility.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED (ε = 0.62 > 0.46). The meritocracy theater constraint resolves the mandatrophy by showing that the snare classification (claimed_type) is not a mere failure of meritocratic selection, but the intended function of the meritocracy narrative. A true rope would acknowledge coordination benefits for all parties and adjust selection mechanisms to reduce extraction asymmetry. A true mountain would be independent of institutional structure. The meritocracy theater is a snare precisely because it performs coordination (transparent standards, predictable selection) while delivering extraction (opportunity concentration, cost shifting, blame assignment). The extracted value flows to: (1) incumbent cohorts who capture opportunity concentration; (2) credential gatekeepers who extract tuition and testing fees while maintaining scarcity; (3) institutional administrators who gain autonomy through legitimacy and reduced pressure for access expansion. The suppression mechanism is internalized: excluded populations accept the meritocracy narrative and attribute failure to personal inadequacy rather than institutional design. The constraint's persistence depends on this suppression remaining opaque — once excluded populations recognize shared structural barriers, coalition formation becomes possible and the constraint's functional extraction capacity degrades. The measurement trajectory (rising theater_ratio, rising extractiveness, rising suppression_requirement) shows the constraint in the middle of its lifecycle: function is declining (selection mechanisms correlate increasingly with privilege, not capability), so performance intensity must increase (more elaborate procedures, more sophisticated legitimacy narratives) to maintain suppression. This is the classic piton/snare boundary dynamics: the constraint is beginning to degrade from pure snare (when institutional faith in meritocracy was high) toward piton (when the constraint persists primarily through institutional inertia and performative maintenance). The mandatrophy is resolved by recognizing that the constraint's essence is the suppression mechanism (internalized blame assignment), not the selection mechanism itself. If suppression is removed (through structural visibility, comparative demographic analysis, coalition formation), the constraint loses extractive force and must either reform toward true rope (acknowledging access barriers and reducing them) or degrade to piton (persist as hollow ritual). Meritocracy theater cannot remain a pure snare once the suppression mechanism is breached.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    merit_definition_contingency,
    'What constitutes ''merit'' in a given selection context, and is this definition genuinely neutral or does it encode incumbent advantage?',
    'Comparative analysis: measure correlation between selection criteria and actual job/program performance; identify variation in merit definition across institutions serving different populations; analyze whose background characteristics predict high scores on merit measures',
    'If merit definition is neutral and predictive: constraint is closer to legitimate rope or weak tangled_rope. If merit definition correlates with privilege markers and predicts outcomes below performance utility: constraint is snare disguised as mountain. If definition varies strategically across contexts: constraint is extraction mechanism with adaptive legitimacy framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(merit_definition_contingency, empirical, 'Whether merit definitions are neutral or encode structural privilege').

omega_variable(
    structural_access_visibility,
    'Do excluded populations perceive their barrier as structural inequality or internalize it as personal inadequacy?',
    'Comparative survey: aspirants'' attribution of failure (structural vs individual); correlation between education about structural barriers and collective action formation; analysis of aspirant narratives before/after exposure to demographic data on cohort success rates',
    'If internalization dominates: suppression is highly effective, extraction persists unchallenged. If structural attribution dominates: coalition formation becomes possible, suppression mechanism fails. Perceptual gap is diagnostic of suppression mechanism strength — internalized barriers are higher than material barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_access_visibility, empirical, 'Whether excluded populations perceive barriers as structural or personal').

omega_variable(
    credential_transfer_utility,
    'Do credentials obtained through meritocratic selection mechanisms actually enable mobility proportional to effort invested, or does post-credential access also depend on non-merit structural factors?',
    'Longitudinal tracking: compare mobility trajectories for credential-holders from privileged vs excluded backgrounds; measure whether selection institution type, social network, and family capital predict outcomes independent of credential quality',
    'If credentials enable proportional mobility: meritocracy constraint functions as rope or weak tangled_rope (genuine coordination with asymmetric distribution). If credentials are necessary but insufficient (post-credential access depends on privilege): constraint is snare (extraction mechanism disguised as coordination). If credential utility correlates with background: meritocracy performs legitimacy for outcomes determined elsewhere.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credential_transfer_utility, empirical, 'Whether credentials enable mobility proportional to effort invested').

omega_variable(
    institutional_opacity_intentionality,
    'Is the lack of transparency around selection criteria (test score weightings, holistic review criteria, implicit cutoffs) a systemic effect of complexity or an intentional mechanism to prevent audit?',
    'Institutional analysis: document communication of selection criteria to applicants vs actual decision-making process; compare clarity levels across institutions; analyze whether institutions that increased transparency saw institutional pressure to change criteria; track institutional response to external audits of selection bias',
    'If opacity is incidental: reform (transparency, bias audits) might reduce extraction. If opacity is integral to the constraint''s function: institutional resistance to transparency will be strong, and transparency efforts will be performative. High institutional opacity + strong resistance to audits + strategic information asymmetry = snare-level suppression mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_opacity_intentionality, empirical, 'Whether selection opacity serves suppression function').

omega_variable(
    alternative_allocation_viability,
    'Are there demonstrable alternative selection mechanisms (lottery, random sampling, needs-based allocation) that produce outcomes (institutional function, social mobility) comparable to meritocratic selection while reducing extraction?',
    'Comparative case studies: institutions using alternative allocation methods vs merit-based peers; controlled experiments (randomized admission cohorts); analysis of outcomes under different allocation rules in similar contexts',
    'If alternatives exist and produce comparable outcomes: meritocracy theater is contingent, not necessary — constraint could be reformed. If alternatives fail or produce worse outcomes: meritocracy constraint has genuine functional necessity. If alternatives exist but are actively suppressed or underfunded: institutional actors are defending extraction mechanism, not coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_allocation_viability, empirical, 'Whether viable alternative selection mechanisms exist').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(meritocracy_theater, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meritocracy_tr_t0, meritocracy_theater, theater_ratio, 0, 0.68).
narrative_ontology:measurement(meritocracy_tr_t5, meritocracy_theater, theater_ratio, 5, 0.75).
narrative_ontology:measurement(meritocracy_tr_t10, meritocracy_theater, theater_ratio, 10, 0.81).

% Extraction over time
narrative_ontology:measurement(meritocracy_be_t0, meritocracy_theater, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(meritocracy_be_t5, meritocracy_theater, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(meritocracy_be_t10, meritocracy_theater, base_extractiveness, 10, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(meritocracy_su_t0, meritocracy_theater, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(meritocracy_su_t5, meritocracy_theater, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(meritocracy_su_t10, meritocracy_theater, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(meritocracy_theater, identity_coordination).
narrative_ontology:boltzmann_floor_override(meritocracy_theater, 0.12).
narrative_ontology:affects_constraint(meritocracy_theater, credentialism_rent_extraction).
narrative_ontology:affects_constraint(meritocracy_theater, intergenerational_wealth_transmission).
narrative_ontology:affects_constraint(meritocracy_theater, epistemic_closure_privileged_cohorts).
narrative_ontology:affects_constraint(meritocracy_theater, opportunity_concentration_mechanisms).

% DUAL FORMULATION NOTE:
% Meritocracy theater is a parent constraint affecting multiple downstream institutional mechanisms. Credentialism rent extraction is a direct sub-mechanism (credential scarcity created by meritocratic filtering enables gatekeeping rent). Intergenerational wealth transmission operates through the same channel (meritocracy narrative legitimizes inherited advantage). Epistemic closure among privileged cohorts is both caused by and reinforces meritocracy theater (belief in personal merit prevents awareness of structural advantage). Opportunity concentration mechanisms are the functional substrate of meritocracy theater (the constraint's extractive effect is opportunity concentration disguised as merit-based allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(meritocracy_theater, institutional, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
