% ============================================================================
% CONSTRAINT STORY: intelligence_source_protection_norms
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_intelligence_source_protection_norms, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: intelligence_source_protection_norms
 *   human_readable: Intelligence Source Protection Norms
 *   domain: national_security/intelligence/governance
 *
 * SUMMARY:
 *   Intelligence source protection norms create a structural extraction
 *   mechanism masked as a security coordination requirement. The constraint
 *   operates across multiple institutional levels: the intelligence community
 *   needs operational security to recruit informants; the judiciary needs
 *   evidence to prosecute threats; prosecutors need witnesses to secure
 *   convictions; sources need protection from retaliation; and defendants
 *   need access to evidence for fair trial. These genuine coordination
 *   problems are real, but the institutional machinery for protecting sources
 *   has accumulated mechanisms that prevent meaningful judicial review of
 *   whether protection is actually necessary or whether it shields
 *   prosecutorial overreach. The constraint exhibits high suppression (68%)
 *   because classified evidence is inherently unreviewable, high theater
 *   (58%) because in camera procedures maintain the appearance of judicial
 *   oversight while deferring to executive judgment, and moderate
 *   extractiveness (52%) because the coordination benefit is genuine but
 *   asymmetrically distributed — sources and prosecutors benefit more than
 *   defendants or oversight bodies.
 *
 * KEY AGENTS:
 *   - Intelligence Agencies: Primary beneficiary (institutional/arbitrage) — can deploy sources without disclosure risk; maintain operational secrecy with judicial deference
 *   - Prosecutors: Secondary beneficiary (institutional/arbitrage) — use classified sources for convictions; avoid cross-examination without disclosure
 *   - Protected Sources: Bifurcated (powerless/trapped victim + institutional/arbitrage beneficiary) — need protection but trapped by protection relationship and retaliation threat
 *   - Prosecuted Defendants: Primary victim (powerless/trapped) — cannot access or challenge evidence against them; trial rights suspended by classification
 *   - Civil Liberties Advocates: Organized constraint recipient (moderate/constrained) — recognize security necessity but constrained from challenging doctrine by national security deference
 *   - Judiciary: Institutional gatekeeper (organized/constrained) — bound by CIPA procedures that prevent meaningful review; constrained by national security exceptionalism
 *   - Intelligence Oversight Bodies: Reform agents (organized/mobile) — building alternatives through declassification timelines, source-neutral evidence, and structural reforms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(intelligence_source_protection_norms, 0.52).
domain_priors:suppression_score(intelligence_source_protection_norms, 0.68).
domain_priors:theater_ratio(intelligence_source_protection_norms, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(intelligence_source_protection_norms, extractiveness, 0.52).
narrative_ontology:constraint_metric(intelligence_source_protection_norms, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(intelligence_source_protection_norms, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(intelligence_source_protection_norms, tangled_rope).
narrative_ontology:human_readable(intelligence_source_protection_norms, "Intelligence Source Protection Norms").
narrative_ontology:topic_domain(intelligence_source_protection_norms, "national_security/intelligence/governance").

domain_priors:requires_active_enforcement(intelligence_source_protection_norms).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(intelligence_source_protection_norms, intelligence_agencies).
narrative_ontology:constraint_beneficiary(intelligence_source_protection_norms, source_safety).
narrative_ontology:constraint_victim(intelligence_source_protection_norms, public_oversight).
narrative_ontology:constraint_victim(intelligence_source_protection_norms, prosecutorial_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROSECUTED DEFENDANT (SNARE) — Faces conviction based on classified evidence they cannot see, challenge, or refute. Cannot exit the trial, cannot access the basis of charges against them. Maximum extraction with zero alternative.
constraint_indexing:constraint_classification(intelligence_source_protection_norms, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROTECTED SOURCE AT RISK (SNARE) — Lives under threat from adversary retaliation if identity exposed. Cannot exit the source relationship without abandoning security guarantees. Trapped by both protection obligations and retaliation threats.
constraint_indexing:constraint_classification(intelligence_source_protection_norms, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CIVIL LIBERTIES ADVOCATE (TANGLED ROPE) — Genuine coordination function: source protection enables infiltration of terrorist networks and disruption of genuine threats, which serves public security. But extraction mechanism: the same norms prevent judicial review of whether source protection is actually necessary or whether it masks prosecutorial overreach. Constrained by institutional barriers to challenging security determinations.
constraint_indexing:constraint_classification(intelligence_source_protection_norms, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INTELLIGENCE AGENCY (ROPE) — Experiences source protection as pure coordination: cannot recruit informants without credible protection; cannot conduct effective counterintelligence without secrecy. The constraint solves the genuine problem of maintaining asset networks. Net beneficiary with arbitrage options — can disclose sources selectively to allied agencies.
constraint_indexing:constraint_classification(intelligence_source_protection_norms, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIARY (TANGLED ROPE) — Genuine coordination function: source protection enables criminal prosecution of terrorism, espionage, organized crime without burning assets. But extraction: source protection doctrine prevents in camera review of whether a particular source's protection claim is legitimate, shifting verification burden from intelligence agency to court. Constrained by national security deference doctrine.
constraint_indexing:constraint_classification(intelligence_source_protection_norms, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CLASSIFIED PROCEDURES ACT INFRASTRUCTURE (PITON) — The institutional apparatus (CIPA procedures, security clearance vetting, in camera review) has become largely performative. The Court grants source protection claims at rates exceeding 95% without meaningful factual challenge. The procedures maintain the appearance of judicial review while deferring to executive judgment in practice. Theater ratio elevated because the institutional machinery goes through motions without substantive gatekeeping.
constraint_indexing:constraint_classification(intelligence_source_protection_norms, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: INTELLIGENCE OVERSIGHT REFORM COALITION (SCAFFOLD) — Organized actors (inspectors general, special counsel offices, intelligence committees) are building alternative verification pathways: declassification timelines, source-neutral prosecution alternatives (signals intelligence, documentary evidence), and structured in camera review protocols that shift burden back to intelligence agencies. These mechanisms have sunset logic — as alternatives mature, reliance on classified evidence should decline. Mobile exit options for reform advocates who see alternatives.
constraint_indexing:constraint_classification(intelligence_source_protection_norms, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN CLAIMED) — Some aspects of source protection appear immutable: adversary retaliation is a real threat; operational security requires information asymmetry; no court can perfectly verify factual necessity without the full intelligence picture. However, the structural data contradicts the mountain classification — the 52% extractiveness, high suppression, and organized reform movement reveal that much of what appears 'inherent' is actually the institutional arrangement of source protection doctrine, which is contingent and contestable.
constraint_indexing:constraint_classification(intelligence_source_protection_norms, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intelligence_source_protection_norms_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(intelligence_source_protection_norms, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intelligence_source_protection_norms, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(intelligence_source_protection_norms, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(intelligence_source_protection_norms, TR),
    TR >= 0.70.

:- end_tests(intelligence_source_protection_norms_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. The constraint extracts from defendants' due process rights, from oversight bodies' review capacity, and from public transparency. But the extraction is not maximal (snare-level) because genuine security coordination creates some defensive justification. The value reflects accumulated doctrinal expansion: source protection began as targeted operational security and has become a general shield against judicial review. Suppression (0.68): High. Classified evidence is inherently unreviewable; defendants cannot mount an effective challenge without accessing the evidence they need to challenge. Information asymmetry prevents meaningful suppression reduction. Theater ratio (0.58): Moderate-high. CIPA procedures (in camera review, security clearance vetting, protective orders) maintain the appearance of judicial oversight while courts grant source protection claims at rates exceeding 95%. The machinery performs its gatekeeping function without substantively exercising it. Extractiveness has increased over the 30-year interval as classified evidence use has expanded and judicial deference has become entrenched doctrine.
 *
 * PERSPECTIVAL GAP:
 *   The widest gap is between prosecutors (Rope perspective: pure coordination solving the recruitment problem) and defendants (Snare perspective: total suppression with no defense). Both operate under the same institutional rules, but their structural positions create opposite classifications. A secondary gap exists between the judiciary's claimed authority (Tangled Rope: gatekeeping both security and fairness) and actual practice (Piton: performative review with >95% approval rate). The reform coalition's Scaffold perspective is prospective — it sees alternative evidence pathways that don't yet fully exist, creating tension with the institutional present. The natural law perspective risks naturalizing the institutional choice (classification of information) as an immutable fact (the information is inherently dangerous).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's effective extractiveness chi derives from d (directionality), f(d) (sigmoid transformation), and scope modifier σ(S). Defendants: d ≈ 0.95 (full target) + trapped exit → f(d) ≈ 1.42 × σ(national=1.0) = high chi. Intelligence agencies: d ≈ 0.05 (full beneficiary) + arbitrage exit → f(d) ≈ -0.12 × σ(national=1.0) = negative chi (they experience the constraint as beneficial). Prosecutors: d ≈ 0.10 (beneficiary) + arbitrage exit → low chi. The judiciary: d ≈ 0.50 (symmetric) + constrained exit → f(d) ≈ 0.65 × σ(national=1.0) = moderate chi. Reform coalition: d ≈ 0.55 (target but with exit path) + mobile exit → f(d) ≈ 0.75 but diminished by visible exit options. No directionality overrides are needed — the structural data produces the right d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that source protection serves both genuine coordination (recruitment, operational security) and extractive mechanisms (suppression of defense evidence, prevention of oversight). The Tangled Rope classification captures this hybrid: beneficiaries (prosecutors, intelligence agencies) experience the coordination function that solves real operational problems; victims (defendants, oversight bodies) experience the extraction mechanism that prevents review. The piton classification at the civilizational level reveals that the institutional machinery (CIPA procedures) has become largely performative — the procedures exist to appear to gatekeep but in practice defer systematically to executive judgment. The scaffold classification is prospective rather than actual — it assumes that alternative evidence pathways will mature sufficiently to bypass reliance on classified sources. Mandatrophy resolution requires asking not 'is source protection coordination or extraction?' but 'for whom and under what conditions is each function dominant?' The answer is: it is coordination for security purposes, but the boundary between security necessity and prosecutorial overreach has become systematically blurred.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    source_necessity_verifiability,
    'Can courts verify whether a particular source''s protection is actually necessary for operational security, or does the information asymmetry prevent meaningful review?',
    'Historical analysis of sources disclosed after trial: were subsequent operational failures linked to disclosure? Comparison of prosecutions using protected vs revealed sources on recidivism and security outcomes.',
    'If courts can verify: Tangled Rope classification holds and in camera review is genuinely protective. If courts cannot verify: classification shifts to Snare — suppression is structural and irreducible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(source_necessity_verifiability, empirical, 'Whether courts can meaningfully verify source protection necessity').

omega_variable(
    alternative_evidence_sufficiency,
    'For national security prosecutions, how many cases could be prosecuted effectively using non-classified evidence (signals intelligence, documentary records, financial trails) instead of human sources?',
    'Audit of closed national security prosecutions: categorize by evidence type used; simulate prosecution scenarios using alternative evidence sources; analyze conviction rates by evidence composition.',
    'If >60% could use alternatives: source protection becomes discretionary, not necessary — classification shifts to Snare as extraction mechanism. If <30%: source protection is genuine coordination requirement — classification confirmed as Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_evidence_sufficiency, empirical, 'Whether alternative evidence could replace protected source testimony').

omega_variable(
    source_retaliation_incidence,
    'What is the actual frequency of adversary retaliation against disclosed sources compared to theoretical threat models?',
    'Longitudinal tracking of disclosed sources in declassified cases; incidence of actual harm vs predicted harm; comparison across jurisdictions with different disclosure regimes.',
    'If retaliation rate <5%: source protection doctrine is oversized relative to actual threat — extraction mechanism revealed. If retaliation rate >40%: threat is real and source protection is legitimate coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(source_retaliation_incidence, empirical, 'Actual frequency of source retaliation post-disclosure').

omega_variable(
    institutional_capture_severity,
    'Has source protection doctrine been systematized to enable prosecutorial overreach independent of security necessity — using classification as shield against judicial review?',
    'Comparative analysis: jurisdictions with mandatory source disclosure vs classified procedures; prosecution success rates by disclosure regime; defendant exoneration rates post-DNA; judicial dissent rates in source protection motions.',
    'If overreach is systematic: suppression mechanism is primarily institutional capture, not security necessity — classification shifts to Snare with extractive overlay. If overreach is marginal: suppression is security-driven — classification confirmed as defensive Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_severity, empirical, 'Whether source protection doctrine enables prosecutorial overreach').

omega_variable(
    sunset_mechanism_feasibility,
    'Can source protection claims be time-limited without compromising long-term source safety (e.g., mandatory sunset after trial, with exceptions for active operations)?',
    'Pilot programs with limited-duration source protection; incidence of post-sunset harm to sources; operational security impact; prosecutorial adaptation to sunset constraints.',
    'If sunset is feasible: Scaffold classification is legitimate and reform trajectory is real. If sunset undermines source safety: suppression is permanent and reform is illusory — piton classification is more accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_mechanism_feasibility, empirical, 'Feasibility of time-limited source protection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(intelligence_source_protection_norms, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inte_tr_t0, intelligence_source_protection_norms, theater_ratio, 0, 0.42).
narrative_ontology:measurement(inte_tr_t15, intelligence_source_protection_norms, theater_ratio, 15, 0.5).
narrative_ontology:measurement(inte_tr_t30, intelligence_source_protection_norms, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(inte_be_t0, intelligence_source_protection_norms, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(inte_be_t15, intelligence_source_protection_norms, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(inte_be_t30, intelligence_source_protection_norms, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(intelligence_source_protection_norms, enforcement_mechanism).
narrative_ontology:affects_constraint(intelligence_source_protection_norms, prosecutorial_accountability).
narrative_ontology:affects_constraint(intelligence_source_protection_norms, classified_information_access_rights).
narrative_ontology:affects_constraint(intelligence_source_protection_norms, national_security_exceptionalism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
