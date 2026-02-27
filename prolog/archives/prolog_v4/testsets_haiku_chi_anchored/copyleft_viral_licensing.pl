% ============================================================================
% CONSTRAINT STORY: copyleft_viral_licensing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyleft_viral_licensing, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: copyleft_viral_licensing
 *   human_readable: Copyleft Viral Licensing (e.g., GPL)
 *   domain: technological/legal
 *
 * SUMMARY:
 *   Copyleft is a legal mechanism that uses copyright law to enforce software
 *   freedom. A copyleft license (most famously the GNU General Public
 *   License) requires that derivative works and linked software also be
 *   released under the same free license. This creates a 'viral' obligation:
 *   if proprietary software incorporates GPL code, that proprietary product
 *   must itself become free software. Copyleft exhibits hybrid properties: it
 *   coordinates free software development through transparency and forced
 *   contribution, but it also extracts compliance costs from vendors who wish
 *   to incorporate free code without releasing their own. The constraint's
 *   extractiveness (0.38) reflects moderate asymmetry — proprietary vendors
 *   bear real costs, but those costs are conditional on choosing to use GPL
 *   code. Suppression (0.42) reflects meaningful barriers: legal liability,
 *   code auditing requirements, and license incompatibility. Theater ratio
 *   (0.35) reflects that the GPL is substantially functional, not
 *   performative — copyleft enforcement has real bite in legal jurisdiction.
 *   The constraint decomposes into seven distinct perspectives: free
 *   developers see pure coordination (Rope), proprietary vendors see
 *   extraction (Snare), enforcement coalitions see hybrid
 *   coordination-coercion (Tangled Rope), permissive licenses represent an
 *   alternative sunset pathway (Scaffold), end users experience paradoxical
 *   freedom with supply chain costs (Tangled Rope), institutional actors use
 *   GPL as performative identity (Piton), and civilizational observers risk
 *   naturalizing copyright as law.
 *
 * KEY AGENTS:
 *   - Free Software Community: Primary beneficiary (moderate/mobile) — gains coordinated development, guaranteed future access, prevented vendor lock-in
 *   - Open Source Developers: Beneficiary + victim (moderate/mobile) — benefit from free upstream code, constrained by reciprocity requirement
 *   - Proprietary Software Vendors: Primary victim (powerful/constrained) — face forced-disclosure requirement if incorporating GPL code; cannot use GPL without releasing proprietary software
 *   - Free Software Foundation / Copyleft Coalition: Enforcer (organized/constrained) — active defense of GPL through litigation threat and community norm enforcement; benefits from expanded free software ecosystem
 *   - End Users: Secondary victim + beneficiary (powerless/trapped) — guaranteed software freedom but trapped in complex license landscape; cannot migrate between incompatible free licenses
 *   - Permissive License Ecosystem: Alternative pathway (institutional/arbitrage) — MIT, Apache 2.0, BSD licenses offer coordination without reciprocity, gradually replacing GPL's functional role
 *   - Corporate Adopters (Red Hat, Canonical, Debian): Institutional actors (institutional/arbitrage) — benefit from GPL's guarantee of source availability for security auditing and customization; navigate copyleft as business model
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyleft_viral_licensing, 0.38).
domain_priors:suppression_score(copyleft_viral_licensing, 0.42).
domain_priors:theater_ratio(copyleft_viral_licensing, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyleft_viral_licensing, extractiveness, 0.38).
narrative_ontology:constraint_metric(copyleft_viral_licensing, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(copyleft_viral_licensing, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyleft_viral_licensing, tangled_rope).
narrative_ontology:human_readable(copyleft_viral_licensing, "Copyleft Viral Licensing (e.g., GPL)").
narrative_ontology:topic_domain(copyleft_viral_licensing, "technological/legal").

domain_priors:requires_active_enforcement(copyleft_viral_licensing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyleft_viral_licensing, free_software_community).
narrative_ontology:constraint_beneficiary(copyleft_viral_licensing, downstream_derivative_authors).
narrative_ontology:constraint_beneficiary(copyleft_viral_licensing, end_users_of_open_source).
narrative_ontology:constraint_victim(copyleft_viral_licensing, proprietary_software_vendors).
narrative_ontology:constraint_victim(copyleft_viral_licensing, closed_source_derivative_authors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPEN SOURCE DEVELOPER (ROPE) — Benefits from free access to upstream code and the coordination mechanism of copyleft (forced transparency, community contribution). Can fork or migrate to non-copyleft licenses if desired (mobile exit). d≈0.35, f(d)≈0.30, σ=1.2 → χ≈0.14. Low effective extraction; pure coordination benefit.
constraint_indexing:constraint_classification(copyleft_viral_licensing, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: PROPRIETARY VENDOR (SNARE) — Wants to incorporate GPL-licensed code without releasing source. Constrained exit: must either (a) avoid GPL code entirely, (b) release proprietary code under GPL (unacceptable), or (c) risk legal liability. d≈0.88, f(d)≈1.20, σ=1.2 → χ≈0.55. Moderate-high effective extraction from vendor perspective.
constraint_indexing:constraint_classification(copyleft_viral_licensing, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: FREE SOFTWARE COALITION (TANGLED ROPE) — Organized agents (FSF, Software Freedom Conservancy, Linux Foundation) enforce and defend copyleft through both coordination (GPL tooling, community norms) and coercive mechanisms (license enforcement, legal threats). d≈0.42, f(d)≈0.45, σ=1.2 → χ≈0.21. Beneficiaries include the coalition itself; victims include non-compliant vendors.
constraint_indexing:constraint_classification(copyleft_viral_licensing, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PERMISSIVE LICENSE ECOSYSTEM (SCAFFOLD) — Permissive licenses (MIT, Apache, BSD) offer coordination benefits without enforced reciprocity, creating an alternative pathway to free software. χ≤0.30. Permissive ecosystems are cannibalizing GPL's functional role, especially for libraries and infrastructure. The GPL's enforcement advantage diminishes as more projects adopt permissive licenses. Sunset mechanism: as open-source norms mature and become industry standard (2010s onward), the need for viral enforcement declines.
constraint_indexing:constraint_classification(copyleft_viral_licensing, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: END USER / SUPPLY CHAIN VICTIM (TANGLED ROPE) — End users benefit from GPL's guarantee that their software is free (transparency, no vendor lock-in, right to modify). But they are trapped in a paradox: they have no exit option if they wish to use derived products, and they bear the cost of vendor non-compliance and fragmentation. d≈0.68, f(d)≈1.02, σ=1.2 → χ≈0.47. Mixed: coordination benefit (freedom guarantee) + extraction cost (license complexity, supply chain fragility).
constraint_indexing:constraint_classification(copyleft_viral_licensing, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: GPL INSTITUTIONAL RITUAL (PITON) — The GPL license text is often copied without full understanding. Developers declare GPL compliance performatively ('I'm open source') without enforcement mechanisms or genuine copyleft defense. theater_ratio≈0.65 reflects that much GPL deployment is institutional theater masking permissive behavior. The license persists through legacy inertia and ideological commitment, not through active enforcement.
constraint_indexing:constraint_classification(copyleft_viral_licensing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / COPYRIGHT AS NATURAL LAW (MOUNTAIN) — From a universal perspective, copyright law itself is treated as an immutable constraint. Copyleft simply inverts copyright's asymmetry. But the structural data (ε=0.38, suppression=0.42, theater=0.35) reveals this as a false summit: copyright is a contingent legal regime, and copyleft is a deliberate strategic deployment of that regime, not a natural law.
constraint_indexing:constraint_classification(copyleft_viral_licensing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyleft_viral_licensing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(copyleft_viral_licensing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(copyleft_viral_licensing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(copyleft_viral_licensing, TR),
    TR >= 0.70.

:- end_tests(copyleft_viral_licensing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The GPL extracts compliance costs from proprietary vendors, but extraction is not severe because it applies only to code actually incorporating GPL software. Vendors can avoid extraction entirely by not using GPL code. The extraction is structural (forced disclosure) but conditional (choice to use code). Unlike a true Snare (inescapable), proprietary vendors have a clear exit: don't use GPL. Suppression (0.42): Moderate. Significant barriers exist: legal liability, code auditing complexity, license incompatibility, and social pressure from the free software community. But suppression is not total — many vendors successfully navigate copyleft by forking code, using compatibility layers, or choosing permissive alternatives. Theater ratio (0.35): Low-moderate. The GPL is substantially functional: it is actively enforced through litigation and community norms, license text is widely understood by developers, and compliance is a real governance mechanism, not ritual. The theater ratio increases over time (0.20 → 0.35) as GPL becomes more institutionalized and less contentious — some organizations adopt GPL performatively without deep commitment.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a dramatic perspectival gap. The free software developer sees Rope (coordination) — copyleft ensures that improvements to shared code benefit the whole community. The proprietary vendor sees Snare (extraction) — they are forced to choose between incorporating GPL code and maintaining proprietary status. The free software coalition sees Tangled Rope (coordination + enforcement) — they defend the license through both community practices and legal action. The permissive license ecosystem sees Scaffold (temporary coordination problem being solved) — alternatives like MIT and Apache provide freedom guarantees without forced reciprocity, gradually making GPL's enforcement mechanism obsolete. The end user sees Tangled Rope (mixed benefit/cost) — they gain freedom guarantees but are trapped in a complex license landscape. The institutional GPL user sees Piton (performative ritual) — GPL becomes a badge of respectability without deep engagement with copyleft's enforcement mechanism. The civilizational observer risks seeing Mountain (natural law of software evolution) but the structural data reveals this as false: copyright itself is contingent.
 *
 * DIRECTIONALITY LOGIC:
 *   Free software community: Beneficiary + mobile → d≈0.25, f(d)≈0.15. Low effective extraction because beneficiaries can exit if needed. Proprietary vendors: Victim + constrained → d≈0.88, f(d)≈1.20. High extraction because vendors face real costs (legal liability, source disclosure, code auditing) and have limited exit options (don't use the code, or accept GPL). Free software coalition: Enforcer + constrained → d≈0.42, f(d)≈0.45. Moderate extraction; coalition has agency and benefits from enforcement. End users: Beneficiary + victim + trapped → d≈0.68, f(d)≈1.02. Mixed: they benefit from software freedom but are trapped in a paradox (cannot easily migrate between incompatible GPL versions or switch to proprietary if needed). Permissive ecosystem: Alternative pathway, institutional + arbitrage → d≈0.15, f(d)≈0.02. Near-zero effective extraction because permissive licenses don't enforce reciprocity. Corporate adopters: Institutional + arbitrage → d≈0.10, f(d)≈-0.08. Negative extraction (net beneficiary) from copyleft's transparency guarantee.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: Copyleft resolves the mandatrophy by distinguishing conditional extraction (Tangled Rope) from coercive extraction (Snare). The GPL is not a pure Snare because (1) vendors can avoid it entirely by not using GPL code, and (2) the enforcement mechanism has genuine coordination benefits (forced transparency, community contribution, prevented vendor lock-in). The GPL is not a pure Rope because (3) proprietary vendors bear real extraction costs, and (4) the license is designed specifically to constrain proprietary behavior. Copyleft is therefore definitively Tangled Rope: it provides coordination (free software community, transparency, prevented fragmentation) AND asymmetric extraction (proprietary vendors cannot incorporate without releasing source). The mandatrophy is resolved by recognizing that the fairness of the extraction depends on one's position in the software ecosystem. From the free software community's perspective, copyleft is fair reciprocity (you benefit from free code, so your improvements should be free too). From the proprietary vendor's perspective, copyleft is unjust forced disclosure. Both perspectives are structurally correct — the constraint IS extraction from the vendor's position, and IS coordination from the free community's position. The analytical question is: is enforced reciprocity a legitimate coordination mechanism, or is it coercive extraction? This is a preference-class omega, not an empirical question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    copyright_legal_validity,
    'What jurisdictions recognize GPL enforcement as legally binding, and do courts actually enforce viral redistribution requirements?',
    'Meta-analysis of GPL litigation outcomes; survey of enforcement success rates across jurisdictions; analysis of settlements vs court victories',
    'If enforcement is weak (< 30% of violations result in compliance): GPL reverts to Piton (performative). If strong (> 70%): GPL maintains Snare classification for vendors. If mixed by jurisdiction: GPL decomposes into multiple constraint stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(copyright_legal_validity, empirical, 'Whether GPL is legally enforceable across jurisdictions').

omega_variable(
    permissive_license_substitution,
    'Are permissive licenses (MIT, Apache 2.0) functionally replacing GPL''s coordination role in reducing fragmentation and vendor lock-in?',
    'Market share analysis: GPL vs permissive licensing in new projects over time; empirical outcomes for vendor lock-in in permissive vs GPL ecosystems; user freedom metrics (modification capability, auditing access) in each regime',
    'If yes: GPL''s extraction mechanism loses force as alternatives provide coordination without enforcement. Scaffold sunset becomes real. If no: GPL maintains structural advantage. Classification shifts from Scaffold to pure Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permissive_license_substitution, empirical, 'Whether permissive licenses substitute for GPL''s coordination function').

omega_variable(
    collective_action_threshold,
    'What is the minimum network size of GPL developers required to make the reciprocity constraint binding (i.e., to make non-compliance costly enough to deter)?',
    'Historical analysis: GPL''s enforcement power before/after critical mass (circa 1998-2010 Linux adoption inflection). Comparison with permissive licenses that lack network effects. Agent-based modeling of coordination failure thresholds.',
    'If threshold is high (> 50% market share): GPL must be continually defended through active enforcement (remains Tangled Rope/Snare). If low (< 20%): network effects are weak, and GPL is Piton (theater). If crossed: GPL transitions from weak to strong.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_threshold, empirical, 'Critical mass threshold for GPL reciprocity enforcement').

omega_variable(
    software_as_speech_vs_tool,
    'Is software copyright a legitimate protection mechanism for developer autonomy and freedom, or is it a tool for proprietary extraction?',
    'Normative analysis: do GPL developers perceive copyleft as freedom protection or as enforcement mechanism? Comparative analysis with non-copyright-based coordination (technical standards, community norms). Legal/philosophical precedent in copyright doctrine.',
    'If freedom protection: GPL is legitimate Rope/Tangled Rope with moral authority. If tool for extraction: GPL is Snare masquerading as justice. This determines mandatrophy resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(software_as_speech_vs_tool, preference, 'Normative status of copyright-based enforcement in software').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyleft_viral_licensing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copyleft_tr_t0, copyleft_viral_licensing, theater_ratio, 0, 0.2).
narrative_ontology:measurement(copyleft_tr_t5, copyleft_viral_licensing, theater_ratio, 5, 0.28).
narrative_ontology:measurement(copyleft_tr_t10, copyleft_viral_licensing, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(copyleft_be_t0, copyleft_viral_licensing, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(copyleft_be_t5, copyleft_viral_licensing, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(copyleft_be_t10, copyleft_viral_licensing, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyleft_viral_licensing, information_standard).
narrative_ontology:affects_constraint(copyleft_viral_licensing, software_patent_regime).
narrative_ontology:affects_constraint(copyleft_viral_licensing, open_source_supply_chain).
narrative_ontology:affects_constraint(copyleft_viral_licensing, proprietary_software_licensing).

% DUAL FORMULATION NOTE:
% Copyleft is downstream of copyright law and patent regimes but represents a distinct structural constraint. The upstream constraint (copyright itself) has different ε and suppression values reflecting its broader scope; copyleft has ε=0.38 reflecting the conditional extraction specific to software derivation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyleft_viral_licensing, powerful, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
