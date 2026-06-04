% ============================================================================
% CONSTRAINT STORY: progressive_era_amendments__eighteenth_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_progressive_era_amendments__eighteenth_amendment, []).

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
 *   constraint_id: progressive_era_amendments__eighteenth_amendment
 *   human_readable: The Eighteenth Amendment: Constitutional Prohibition
 *   domain: political/legal/constitutional_law
 *
 * SUMMARY:
 *   The Eighteenth Amendment represents the apex and nadir of progressive-era
 *   constitutional ambition: an attempt to use fundamental law for direct
 *   social regulation of personal consumption. Ratified in 1919 and repealed
 *   in 1933, the Amendment lasted fourteen years — the shortest-lived major
 *   constitutional provision in American history. It created a structural
 *   constraint with maximal suppression (alcohol production, distribution,
 *   and sale criminalized by constitutional command) and asymmetric
 *   extraction (the temperance coalition and enforcement bureaucracy
 *   benefited; drinkers, brewers, and immigrant communities bore the costs).
 *   The Amendment's failure instantiates a critical puzzle: why does this
 *   constitutional constraint classify as both an immutable natural law (from
 *   the analytical perspective at civilizational scale) and a pure snare
 *   (from the powerless perspective at biographical scale) when empirically
 *   it was reversed within a generation? The answer reveals the false summit:
 *   constitutional form does not confer immutability; it merely codifies the
 *   will of a supermajority at a specific moment. When that will erodes, the
 *   Amendment becomes structurally vulnerable. The constraint demonstrates
 *   how direct constitutional social regulation can succeed in suppression
 *   (alcohol consumption did fall significantly during prohibition) while
 *   failing in legitimacy (the mechanism generated bootlegging, organized
 *   crime, and enforcement corruption that delegitimized the moral claim).
 *
 * KEY AGENTS:
 *   - Temperance Coalition: Primary beneficiary (organized/mobile) — achieved their stated constitutional goal; experienced constraint as coordination and moral victory, at least initially
 *   - Drinkers and Brewers: Primary victims (powerless/trapped) — criminalized for behavior previously legal; faced unemployment, cultural loss, and legal liability with no legitimate exit
 *   - Immigrant Communities: Secondary victims (powerless/trapped) — beer gardens and saloons were nodes of community life; criminalized through cultural targeting of the liquor trade
 *   - Federal Enforcement Bureaucracy: Beneficiary-victim hybrid (institutional/arbitrage) — gained expanded authority and resources while bearing corruption exposure and institutional inertia pressure
 *   - Criminal Market Ecosystem: Organized victims (organized/constrained) — emerged as enforcement constraint created profitable black markets; extracted rents from alcohol trade while bearing law enforcement violence
 *   - Rule of Law (abstract): Victim (powerless/trapped) — enforcement overreach, warrantless searches, federal power expansion, separation of powers degradation created lasting structural damage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(progressive_era_amendments__eighteenth_amendment, 0.68).
domain_priors:suppression_score(progressive_era_amendments__eighteenth_amendment, 0.85).
domain_priors:theater_ratio(progressive_era_amendments__eighteenth_amendment, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(progressive_era_amendments__eighteenth_amendment, extractiveness, 0.68).
narrative_ontology:constraint_metric(progressive_era_amendments__eighteenth_amendment, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(progressive_era_amendments__eighteenth_amendment, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(progressive_era_amendments__eighteenth_amendment, snare).
narrative_ontology:human_readable(progressive_era_amendments__eighteenth_amendment, "The Eighteenth Amendment: Constitutional Prohibition").
narrative_ontology:topic_domain(progressive_era_amendments__eighteenth_amendment, "political/legal/constitutional_law").

domain_priors:requires_active_enforcement(progressive_era_amendments__eighteenth_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(progressive_era_amendments__eighteenth_amendment, 'd89b09ff-5138-4429-a594-be4b025fa24b').
narrative_ontology:cs_kernel_codification('d89b09ff-5138-4429-a594-be4b025fa24b', formalized).
narrative_ontology:cs_authority_grounding('d89b09ff-5138-4429-a594-be4b025fa24b', extraction).
narrative_ontology:cs_reading_relation('d89b09ff-5138-4429-a594-be4b025fa24b', progressive_era_amendments__nineteenth_amendment, coexists_with).
narrative_ontology:cs_reading_relation('d89b09ff-5138-4429-a594-be4b025fa24b', progressive_era_amendments__seventeenth_amendment, coexists_with).
narrative_ontology:cs_reading_relation('d89b09ff-5138-4429-a594-be4b025fa24b', progressive_era_amendments__sixteenth_amendment, coexists_with).
narrative_ontology:cs_axiom('d89b09ff-5138-4429-a594-be4b025fa24b', foundational, constitutional_direct_consumption_regulation).
narrative_ontology:cs_axiom_status(constitutional_direct_consumption_regulation, overridden).
narrative_ontology:cs_axiom_grounding('d89b09ff-5138-4429-a594-be4b025fa24b', constitutional_direct_consumption_regulation, empirically_contingent).
narrative_ontology:cs_axiom('d89b09ff-5138-4429-a594-be4b025fa24b', foundational, moral_prohibition_requires_constitutional_codification).
narrative_ontology:cs_axiom_status(moral_prohibition_requires_constitutional_codification, overridden).
narrative_ontology:cs_axiom_grounding('d89b09ff-5138-4429-a594-be4b025fa24b', moral_prohibition_requires_constitutional_codification, deontological).
narrative_ontology:cs_reference_frame('d89b09ff-5138-4429-a594-be4b025fa24b', constitutional_permanent_prohibition_regime).
narrative_ontology:cs_drift_state('d89b09ff-5138-4429-a594-be4b025fa24b', repeal_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('d89b09ff-5138-4429-a594-be4b025fa24b', '').
narrative_ontology:cs_kernel_id(progressive_era_amendments__eighteenth_amendment, progressive_era_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(progressive_era_amendments__eighteenth_amendment, temperance_coalition).
narrative_ontology:constraint_beneficiary(progressive_era_amendments__eighteenth_amendment, enforcement_bureaucracy).
narrative_ontology:constraint_victim(progressive_era_amendments__eighteenth_amendment, drinkers).
narrative_ontology:constraint_victim(progressive_era_amendments__eighteenth_amendment, brewers_distillers).
narrative_ontology:constraint_victim(progressive_era_amendments__eighteenth_amendment, rule_of_law).
narrative_ontology:constraint_victim(progressive_era_amendments__eighteenth_amendment, immigrant_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DRINKERS AND BREWERS (SNARE) — Trapped by constitutional command with no legal exit; face criminal liability for behavior previously legal. The constraint extracts labor and wealth toward enforcement machinery while eliminating legitimate commerce. Maximum suppression: alternatives are criminalized, not merely constrained. The brewery worker faces unemployment; the immigrant community loses cultural institutions (beer gardens, saloons). Exit is not costly — it is illegal.
constraint_indexing:constraint_classification(progressive_era_amendments__eighteenth_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOCAL POLITICIANS AND LAW ENFORCEMENT (TANGLED ROPE) — Genuinely coordinate alcohol regulation at the local level (coordination function present) while facing impossible enforcement burden and corruption incentives. The constraint benefits them (enlarged enforcement apparatus, federal funding, moral authority) while extracting costs (bribery vulnerability, corruption, loss of local legitimacy). Neither pure extraction nor pure coordination — both mechanics present.
constraint_indexing:constraint_classification(progressive_era_amendments__eighteenth_amendment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TEMPERANCE COALITION (ROPE) — Organized actors with agency who achieved their stated goal through legitimate democratic process. Experience the constraint as coordination: the Amendment codifies their shared commitment to alcohol prohibition. Low extraction cost to them (they wanted this), high coordination benefit (crystallized their moral position into fundamental law). Mobile exit option because they can disavow the Amendment if it fails — which they eventually did.
constraint_indexing:constraint_classification(progressive_era_amendments__eighteenth_amendment, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: THE CRIMINAL MARKET ECOSYSTEM (SNARE) — Organized crime (bootleggers, speakeasies, smuggling operations) emerges as the constraint's inevitable counterparty. They benefit from high extraction (illegal markup, monopoly rents) while bearing suppression (law enforcement violence, turf wars). The organized crime perspective sees pure extraction — the liquor trade is re-routed into black markets at higher cost, with surplus captured by criminal organizations. This is a snare for crime syndicates as well — exit from the profitable black market is difficult once established.
constraint_indexing:constraint_classification(progressive_era_amendments__eighteenth_amendment, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ENFORCEMENT BUREAUCRACY (PITON) — The Prohibition Bureau maintains its apparatus through institutional inertia and theater even as the Amendment's core function (preventing alcohol consumption) manifestly fails. Agents (federal agents, local police) benefit through salary, job security, and expanded authority. Theater ratio reflects that much enforcement activity is performative — raids on speakeasies provide headline compliance while bootlegging openly continues. The bureaucracy persists not because it works, but because it exists and captures resources.
constraint_indexing:constraint_classification(progressive_era_amendments__eighteenth_amendment, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CONSTITUTIONAL IMMUTABILITY VIEW (MOUNTAIN) — From a civilizational scale, this perspective sees the Eighteenth Amendment as an inevitable constitutional expression of the nation's will at a specific moment, unchangeable except by supermajority amendment. The mountain framing naturalizes the constraint as an immutable feature of the constitutional order — once codified, only another supermajority can reverse it. However, the empirical data contradicts this: the Amendment was repealed within 14 years, showing that constitutional constraints are not immutable when they lose sufficient political support. This is a FALSE SUMMIT — the mountain classification naturalizes what is actually a contingent institutional artifact vulnerable to political reversal.
constraint_indexing:constraint_classification(progressive_era_amendments__eighteenth_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(progressive_era_amendments__eighteenth_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(progressive_era_amendments__eighteenth_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(progressive_era_amendments__eighteenth_amendment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(progressive_era_amendments__eighteenth_amendment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(progressive_era_amendments__eighteenth_amendment, TR),
    TR >= 0.70.

:- end_tests(progressive_era_amendments__eighteenth_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The Amendment extracts through criminalization of consumption and production, creating enforcement labor and criminal market rents. The peak measurement (0.68 at year 8) reflects maximum black market development; the decline to 0.62 at year 12 reflects declining enforcement enthusiasm as repeal became politically inevitable. The measurement trajectory shows extraction accumulation and then decay — structural evidence of institutional inertia and political reversal. Suppression (0.85): Maximal. Constitutional prohibition eliminates legitimate alternatives entirely; all consumption and production are criminal. Suppression is not merely high cost (constrained exit) — it is criminalization (trapped exit). The trajectory from 0.65 to 0.85 reflects escalating enforcement intensity as bootlegging expanded; the decline to 0.72 at repeal reflects enforcement breakdown as law enforcement withdrew from a lost cause. Theater ratio (0.58): Moderate. Federal enforcement activity (raids, arrests, seizures) was partially genuine enforcement and partially performative show. The Prohibition Bureau conducted high-visibility raids to generate compliance headlines, but bootlegging operated openly. The theatrical component increased as genuine suppression failed — enforcement shifted toward visible performance to maintain political legitimacy. Claimed type (snare): The Amendment's classification as snare is robust across multiple perspectives (powerless agents experience it as snare; criminal markets experience it as snare). Only the beneficiary coalition and analytical observer perspectives diverge toward rope and mountain, respectively — those are perspectival artifacts of structural position, not reflections of the constraint's true mechanics.
 *
 * PERSPECTIVAL GAP:
 *   The eighteenth amendment demonstrates the maximal perspectival gap of the constraint corpus. The beneficiary coalition (temperance) experiences rope (coordination of their moral commitment into law). The analytical observer at civilizational scale risks mountain classification (constitutional amendments are immutable law). But the powerless perspective (drinkers, brewers, immigrant communities) experiences pure snare (criminalized with no exit). The criminal market perspective also experiences snare, but from the opposite vector (beneficiary from prohibition's black market rents, trapped by law enforcement). The enforcement bureaucracy experiences piton — their activity persists through institutional inertia long after the constraint's popular support erodes. The perspectival gap reveals that the same structural data yields six distinct classifications depending on observer position and time horizon. The false summit detector fires on the mountain perspective: constitutional amendments are not immutable natural laws; they are reversible institutional arrangements whose durability depends on continuous political support.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality structure (d values derived from beneficiary/victim declarations and exit options) drives the observed perspectival gap. The temperance coalition (beneficiary + mobile exit) derives d ≈ 0.15, producing negative or low f(d) and rope classification. Drinkers (victim + trapped exit) derive d ≈ 0.95, producing maximum f(d) ≈ 1.42 and snare classification. Criminal markets (victim + constrained exit due to law enforcement, but beneficiary from black market rents) derive a mixed d reflecting asymmetric extraction — they benefit from suppression of legitimate competition, but are suppressed themselves by enforcement. The enforcement bureaucracy (beneficiary + arbitrage exit, since they can disavow enforcement if repeal occurs) derives low d but experiences high institutional embedding, producing piton classification. The false summit mountain perspective (analytical + analytical) derives d ≈ 0.72 by default, producing high f(d), but the constitutional form creates the illusion of immutability that pushes classification toward mountain despite the behavioral data contradicting it.
 *
 * MANDATROPHY ANALYSIS:
 *   The Eighteenth Amendment resolves the mandatrophy through temporal decay and political reversal. At ratification (year 0), extractiveness was moderate (0.45) because enforcement capacity was nascent and public compliance was high (moral support for temperance remained broad). As black markets developed (years 4–8), extractiveness peaked (0.68) because suppression costs rose while enforcement intensity increased, creating high extraction combined with high suppression. As repeal momentum built (years 12–14), extractiveness declined (0.62) because enforcement capacity collapsed and political will to maintain the constraint eroded. The measurement trajectory documents the constraint's lifecycle: growth of enforcement apparatus, peak extraction, institutional decay, political reversal, and constitutional repeal. The mandatrophy is resolved by recognizing that the Eighteenth Amendment is a snare that contains the seeds of its own reversal: maximal suppression (elimination of legitimate alternatives) necessarily drives black market extraction, which generates organized crime, which triggers enforcement corruption, which delegitimizes the moral coalition's claim, which produces political coalition for repeal. The constraint is self-defeating — it cannot maintain high suppression without creating criminal market dynamics that undermine its legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_irreversibility_assumption,
    'Is a constitutional amendment an immutable constraint, or a reversible institutional arrangement?',
    'Historical analysis: the Eighteenth Amendment was repealed by the Twenty-first Amendment within 14 years, demonstrating that constitutional constraints can be reversed when political support erodes. Contrast with amendments that persist (13th, 14th, 15th, 19th) to identify what makes some constitutional provisions durable.',
    'If constitutional amendments are contingent on political support: the mountain perspective is a false summit, and the constraint is snare/tangled_rope depending on which agent you measure from. If constitutional amendments are truly immutable once ratified: the mountain classification stands, but empirical data contradicts this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_irreversibility_assumption, empirical, 'Whether constitutional amendments represent immutable constraints or reversible institutional arrangements').

omega_variable(
    temperance_coalition_beneficiary_status,
    'Does the temperance coalition genuinely benefit from the Eighteenth Amendment, or does it bear costs from its inevitable failure and criminal market displacement?',
    'Historical narrative of temperance coalition outcomes: Did supporters feel vindicated (beneficiary), or did they experience the Amendment''s failure as a delegitimizing blow to their moral authority (victim)? Temporal measurement: did beneficiary satisfaction persist or erode as bootlegging expanded?',
    'If coalition bore net costs: beneficiary declaration is incorrect, and the classification changes from rope (beneficiary perspective) to tangled_rope or snare (unexpected extraction). If coalition benefited politically but suffered moral humiliation: mixed outcomes requiring nuanced directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temperance_coalition_beneficiary_status, empirical, 'Whether temperance coalition experienced Amendment as genuine beneficiary or as moral victim').

omega_variable(
    criminal_market_extraction_displacement,
    'Did the Eighteenth Amendment eliminate alcohol extraction, or merely displace it into criminal markets at higher cost?',
    'Economic analysis: comparison of alcohol prices, consumption patterns, and total social cost (law enforcement + black market + health outcomes) under prohibition vs pre-prohibition. If total extraction increased due to black market markups and crime: extractiveness should be higher, not lower. If consumption fell despite black markets: extraction was genuinely suppressed.',
    'If extraction was displaced to higher-cost criminal markets: the Amendment''s extractiveness is higher than the snare classification suggests, and should reclassify as snare with higher χ. If extraction was genuinely suppressed: snare classification is correct, showing how suppression can reduce consumption at the cost of criminalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criminal_market_extraction_displacement, empirical, 'Whether prohibition suppressed alcohol extraction or displaced it into criminal markets').

omega_variable(
    reading_contest_eighteenth_vs_nineteenth,
    'Within the progressive era amendments kernel, does the Eighteenth Amendment''s failure to achieve its stated goal (suppression of drinking) foreclose or coexist with the Nineteenth Amendment''s success in achieving voting rights for women?',
    'Structural comparison: both amendments use constitutional codification to enforce social change. Eighteenth failed (repealed 14 years later); Nineteenth succeeded (persists 100+ years). Does the Eighteenth''s failure show that direct constitutional social regulation doesn''t work (forecloses confidence in the Nineteenth''s mechanism)? Or do the amendments coexist as two different readings of when constitutional codification is appropriate (Nineteenth targets voting rights; Eighteenth targets consumption)?',
    'If Eighteenth forecloses Nineteenth''s mechanism: the Nineteenth Amendment is relying on a strategy that the Eighteenth proved fails. If they coexist: the progressive era used constitutional codification opportunistically for goals with different structural support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_eighteenth_vs_nineteenth, conceptual, 'Structural relationship between Eighteenth and Nineteenth Amendment readings within the kernel').

omega_variable(
    rule_of_law_victim_status,
    'Is the rule of law a victim of the Eighteenth Amendment, or merely a structural casualty of enforcement overreach?',
    'Legal-philosophical analysis: did the Amendment''s enforcement undermine constitutional norms (separation of powers, due process, privacy) in ways that persisted beyond repeal? Did the expansion of federal enforcement apparatus create path dependencies that degraded the rule of law even after prohibition ended?',
    'If rule of law suffered lasting damage: the victim set should include ''constitutional integrity'' alongside material victims. If damage was reversed at repeal: rule of law was constrained but not permanently victimized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rule_of_law_victim_status, conceptual, 'Whether rule of law is a structural victim of Eighteenth Amendment enforcement or a temporary casualty').

omega_variable(
    kernel_codification_form_constraint,
    'Is the Eighteenth Amendment''s form (constitutional amendment) an essential property of this constraint, or could the same prohibition mechanism be instantiated through statute?',
    'Counterfactual: if the same prohibition rules had been enacted as federal statute (Volstead Act without the Amendment), would the constraint''s classification change? Would enforceability be stronger or weaker? Would the victim set remain the same?',
    'If constitutional form is essential: the reading''s distinguishing axiom includes ''constitutional codification is necessary for this moral commitment.'' If constitutional form is incidental: the constraint could be instantiated multiple ways, and the Amendment is a performative choice by the coalition to cement their moral claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_codification_form_constraint, conceptual, 'Whether constitutional codification is an essential property of prohibition or an incidental form choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(progressive_era_amendments__eighteenth_amendment, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eighteenth_tr_t0, progressive_era_amendments__eighteenth_amendment, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eighteenth_tr_t6, progressive_era_amendments__eighteenth_amendment, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(eighteenth_be_t0, progressive_era_amendments__eighteenth_amendment, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(eighteenth_be_t4, progressive_era_amendments__eighteenth_amendment, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(eighteenth_be_t8, progressive_era_amendments__eighteenth_amendment, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(eighteenth_be_t12, progressive_era_amendments__eighteenth_amendment, base_extractiveness, 12, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(eighteenth_su_t0, progressive_era_amendments__eighteenth_amendment, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(eighteenth_su_t4, progressive_era_amendments__eighteenth_amendment, suppression_requirement, 4, 0.78).
narrative_ontology:measurement(eighteenth_su_t8, progressive_era_amendments__eighteenth_amendment, suppression_requirement, 8, 0.85).
narrative_ontology:measurement(eighteenth_su_t12, progressive_era_amendments__eighteenth_amendment, suppression_requirement, 12, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(progressive_era_amendments__eighteenth_amendment, enforcement_mechanism).
narrative_ontology:affects_constraint(progressive_era_amendments__eighteenth_amendment, progressive_era_amendments__sixteenth_amendment).
narrative_ontology:affects_constraint(progressive_era_amendments__eighteenth_amendment, progressive_era_amendments__seventeenth_amendment).
narrative_ontology:affects_constraint(progressive_era_amendments__eighteenth_amendment, progressive_era_amendments__nineteenth_amendment).

% DUAL FORMULATION NOTE:
% The Eighteenth Amendment is part of the progressive era amendments kernel. All four amendments (16th, 17th, 18th, 19th) attempted to use fundamental law to enforce progressive goals at the federal level. The 18th Amendment's failure (repealed within 14 years) contrasts sharply with the durable success of the others. Structurally, the 18th is downstream of temperance movement political organization (the beneficiary coalition had sufficient power to achieve supermajority ratification) but upstream of the criminal market constraint (prohibition created the black market as a downstream effect). The network link indicates constraint family membership, not causal dependence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
