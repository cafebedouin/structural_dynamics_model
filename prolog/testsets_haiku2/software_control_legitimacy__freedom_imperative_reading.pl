% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__freedom_imperative_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: software_control_legitimacy__freedom_imperative_reading
 *   human_readable: Proprietary Software as Freedom Denial (Freedom Imperative Reading)
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This constraint story instantiates the freedom imperative reading of
 *   software control legitimacy: a reading that treats proprietary software
 *   as an illegitimate restriction on user autonomy and asserts that software
 *   control is fundamentally a matter of human freedom, not property. From
 *   this reading's perspective, all proprietary software operates as a snare
 *   — a constraint that extracts computing autonomy from end-users and
 *   dependent organizations while persisting through legal enforcement
 *   (copyright law, Digital Millennium Copyright Act, licensing terms) rather
 *   than through genuine coordination. The reading's beneficiary is
 *   users-as-rights-holders (an analytical category vindicating the principle
 *   of computing freedom); the victims are end-users, dependent
 *   organizations, and future developers denied access to source code. This
 *   reading coexists with three sibling readings of the same kernel: a
 *   pragmatic openness reading (which treats open source as a development
 *   methodology, not a rights imperative), a property rights reading (which
 *   legitimizes vendor control through copyright), and a commons reading
 *   (which seeks negotiated collective governance rather than categorical
 *   freedom). This story authors the freedom imperative reading cleanly and
 *   independently; the other readings are separate constraint stories.
 *
 * KEY AGENTS:
 *   - End users: Trapped in proprietary ecosystems, denied audit and modification rights; powerless individually but collectively the primary freedom-deniers in this reading
 *   - Dependent organizations: Businesses locked into proprietary platforms by identity fusion (enterprise workflows ossified around specific systems); moderately powerful but identity-constrained
 *   - Future developers: Prevented from learning from or building on existing code; constrained by closed-source barriers
 *   - Users-as-rights-holders: Analytical beneficiary representing the principle that users ought to control their computing; not a concrete seat but the vindicated proposition
 *   - Proprietary vendors: Structurally excluded — their business model depends on denying freedom
 *   - Open source community: Observer seat demonstrating the operational possibility of freedom-respecting software
 *   - Regulatory authorities: Observer seat with potential enforcement power if the freedom imperative becomes recognized as a right
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, 0.89).
domain_priors:suppression_score(software_control_legitimacy__freedom_imperative_reading, 0.78).
domain_priors:theater_ratio(software_control_legitimacy__freedom_imperative_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_control_legitimacy__freedom_imperative_reading, "Proprietary Software as Freedom Denial (Freedom Imperative Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__freedom_imperative_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__freedom_imperative_reading, '15321e7e-a07b-4559-aa74-0818a24bdb20').
narrative_ontology:cs_kernel_codification('15321e7e-a07b-4559-aa74-0818a24bdb20', formalized).
narrative_ontology:cs_authority_grounding('15321e7e-a07b-4559-aa74-0818a24bdb20', lineage).
narrative_ontology:cs_interpretation_layer_present('15321e7e-a07b-4559-aa74-0818a24bdb20').
narrative_ontology:cs_reading_relation('15321e7e-a07b-4559-aa74-0818a24bdb20', software_control_legitimacy__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('15321e7e-a07b-4559-aa74-0818a24bdb20', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('15321e7e-a07b-4559-aa74-0818a24bdb20', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('15321e7e-a07b-4559-aa74-0818a24bdb20', foundational, user_computing_autonomy_as_fundamental_right).
narrative_ontology:cs_axiom_status(user_computing_autonomy_as_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('15321e7e-a07b-4559-aa74-0818a24bdb20', user_computing_autonomy_as_fundamental_right, deontological).
narrative_ontology:cs_axiom('15321e7e-a07b-4559-aa74-0818a24bdb20', foundational, software_as_essential_infrastructure).
narrative_ontology:cs_axiom_status(software_as_essential_infrastructure, holdable).
narrative_ontology:cs_axiom_grounding('15321e7e-a07b-4559-aa74-0818a24bdb20', software_as_essential_infrastructure, instrumental).
narrative_ontology:cs_axiom('15321e7e-a07b-4559-aa74-0818a24bdb20', secondary, property_claims_subordinate_to_autonomy_rights).
narrative_ontology:cs_axiom_status(property_claims_subordinate_to_autonomy_rights, holdable).
narrative_ontology:cs_axiom_grounding('15321e7e-a07b-4559-aa74-0818a24bdb20', property_claims_subordinate_to_autonomy_rights, deontological).
narrative_ontology:cs_reference_frame('15321e7e-a07b-4559-aa74-0818a24bdb20', user_computing_autonomy_as_fundamental_right).
narrative_ontology:cs_drift_state('15321e7e-a07b-4559-aa74-0818a24bdb20', contemporary_digital_dependency_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('15321e7e-a07b-4559-aa74-0818a24bdb20', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, users_as_rights_holders).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, end_users).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, dependent_organizations).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, future_developers).
narrative_ontology:constraint_vindicates(software_control_legitimacy__freedom_imperative_reading, user_computing_autonomy).
narrative_ontology:constraint_vindicates(software_control_legitimacy__freedom_imperative_reading, software_as_infrastructure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot modify, audit, repair, or understand the software they depend on daily. Trapped by network effects (everyone uses the same proprietary platform) and switching costs (data lock-in, retraining, incompatibility). Denied fundamental computing autonomy; forced into passive consumption of opaque systems.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, end_users, payer,
    powerless, biographical, trapped, global).

% Businesses and institutions that depend on proprietary software and cannot easily switch or audit their own systems. Their operational survival becomes hostage to vendors' upgrade cycles, licensing terms, and business decisions. Constrained by both economics and organizational identity (enterprise workflows ossified around specific proprietary platforms).
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, dependent_organizations, payer,
    moderate, generational, identity_locked, global).

% Cannot learn from or build upon existing proprietary code; must reinvent solutions in isolation. The closed source barrier prevents cumulative knowledge transmission and slows the entire field's development. Each proprietary system is a dead end rather than a stepping stone.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, future_developers, payer,
    moderate, generational, constrained, global).

% In principle, users possess the fundamental right to control their own computing systems. The freedom imperative reading vindicates this abstract collective claim, even where no concrete seat holds sufficient power to enforce it. Users-as-rights-holders is an analytical beneficiary category representing the vindicated principle, not a concrete seat that captures extraction.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, users_as_rights_holders, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(software_control_legitimacy__freedom_imperative_reading, users_as_rights_holders).

% Software publishers that distribute closed-source software. From this reading's perspective, they are excluded from the legitimate governance conversation because their business model depends on denying user freedom. Their voice is not absent by accident — their core interest (control over code and licensing terms) structurally opposes the freedom imperative and would advocate against it if seated at the table.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_vendors, excluded,
    institutional, generational, mobile, global).

% Developers and organizations practicing free and open-source software development. From this reading's seat, they observe and document the practices that instantiate user freedom. Their role is not to set the constraint but to demonstrate its operational instantiation and measure the gap between proprietary and libre software regimes.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, open_source_community, observer,
    organized, generational, mobile, global).

% Governments and international bodies that may recognize software control as a right and attempt to mandate transparency, auditability, or modification rights for critical software. Their intervention represents a potential enforcement pathway for the freedom imperative, though most contemporary authorities treat software licensing as a property matter, not a rights matter.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__freedom_imperative_reading, proprietary_vendors).
narrative_ontology:fixing_cost_class(software_control_legitimacy__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Theoretically, none from the freedom imperative reading's perspective. This reading rejects proprietary software's framing as coordination. The 'problems' proprietary software claims to solve (developer revenue through licensing fees, copyright protection, vendor investment incentives) are themselves contested as illegitimate when they rest on denying user freedom. The reading interprets the coordination narrative as the cover story the snare uses to justify itself.
% TRANSFER_FUNCTION: Moves computing autonomy (and its attendant value — productivity, data control, security oversight, repair agency) from end-users and dependent organizations to proprietary vendors. Users surrender the ability to modify, audit, repair, and understand their systems in exchange for vendor-mediated access to functionality. Vendors capture the right to set terms unilaterally: what users can do, which upgrades are forced, when support ends, how data is handled.
% ABSENT_VOICES: Proprietary software vendors are structurally excluded: their core interest (maintaining closed control and intellectual property protection) directly opposes the freedom imperative's core premise and would advocate for its rejection. Users themselves are often absent from software governance conversations — their preference for freedom is inferred from the principle rather than elicited from organized representation, especially in corporate and institutional contexts where IT decisions are centralized. Developing-world communities dependent on affordable, modifiable, locally-repairable software are also typically absent from licensing and design decisions made in the Global North venture-capital context.
% DISAPPEARANCE_RATIONALE: If the proprietary software constraint ceased to be enforceable/legitimate, the software ecosystem would reorganize structurally: all code would be open-source and modifiable, user audit and repair rights would be universal defaults, vendor lock-in mechanisms (obfuscation, API restrictions, data format lock-in) would dissolve, and software development would shift to commons-based models, cooperatively-governed platforms, and user-controlled governance. Billions in proprietary software licensing revenue would evaporate; organizational IT workflows would need to be reimplemented or ported to open-source alternatives; the software industry's current business model based on licensing closed code would become inoperable. This is not a marginal adjustment — it is a civilizational restructuring of computing infrastructure.
% FOUNDING_PROBLEM: The foundational problem this reading identifies is not a technical or economic problem requiring a proprietary solution, but a normative one: the question of whether humans have the right to understand and control the machines that mediate their lives. Historically, proprietary software arose not to solve a foundational coordination problem, but from the application of copyright and intellectual property doctrine to software code — a legal choice, not an inevitable technical necessity. The reading argues that in a context of digital dependency and planetary computing infrastructure, the founding problem that matters is user autonomy and the right not to be locked into opaque systems.
% FOUNDING_PROBLEM_CORROBORATION: The freedom imperative reading's claim about the founding problem is corroborated by free software advocates and theorists (Richard Stallman, GNU project documentation, Free Software Foundation), human rights organizations (Article 19, Access Now) that frame digital autonomy as a human right, independent security researchers documenting risks of closed-source dependency, and technologists from the Global South who emphasize the necessity of local control and repair capacity for resource-constrained contexts. This corroboration comes from outside the beneficiary category of proprietary vendors; it includes principled advocates, not just people harmed by the constraint. The founding problem's status is contested because the pragmatic-openness reading and property-rights reading dispute both that user autonomy is the foundational need and that proprietary software denies it in a morally illegitimate way. Vendors and their representatives argue that proprietary control is necessary for sustainable software investment and that users freely choose proprietary systems in competitive markets.
narrative_ontology:disappearance_verdict(software_control_legitimacy__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__freedom_imperative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__freedom_imperative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__freedom_imperative_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__freedom_imperative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__freedom_imperative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.89) because this reading categorically rejects proprietary software's legitimacy claim — from the perspective of the freedom imperative, every proprietary software system extracts user autonomy. There is no legitimate coordination function that justifies this extraction; the 'coordination' story (curated software, intellectual property protection, vendor investment incentives) is the cover story that the snare uses to justify itself. Suppression is high (0.78) because proprietary software's persistence depends on legal enforcement (copyright, DMCA, licensing) and network effects (lock-in), not on voluntary participant preference. Theater ratio is moderate (0.42): vendors perform genuine engineering work and do provide functional value (security updates, feature development), but an increasing share of their activity is devoted to enforcing control (anti-tamper mechanisms, license verification, planned obsolescence) rather than serving users. Accessibility collapse is high (0.72): once proprietary systems are entrenched in critical infrastructure, alternatives become inaccessible due to switching costs, data lock-in, and network effects. Resistance is substantial (0.68): the open source community, free software advocates, and an increasing number of security/privacy advocates actively resist proprietary software's legitimacy claim, even though they remain minoritized. The measurement series shows extraction accumulating slightly over the 40-year interval, suppression hardening as legal and technical enforcement mechanisms mature (DMCA, software licensing litigation, anti-circumvention technologies), and theater increasing as vendors invest more in marketing and obfuscation relative to core functionality.
 *
 * PERSPECTIVAL GAP:
 *   The freedom imperative reading is fundamentally a perspectival choice: it stakes its analysis on the claim that computing autonomy is a human right that overrides property claims. A vendor reading this constraint would classify it very differently (as a snare with low extraction because vendors do not extract — they provide a service users voluntarily purchase; the beneficiary is the vendor, not users). A pragmatist reading would say the extraction claim is category error — proprietary software is a legitimate development choice. The engine computes per-seat classifications from structural data (who benefits, who pays, exit options); this story supplies the structural data for the freedom imperative reading. The divergence from other readings is not a bug — it is the signal that a contested kernel exists. This story does not reconcile the readings; it instantiates one cleanly.
 *
 * DIRECTIONALITY LOGIC:
 *   From the freedom imperative reading's perspective: end-users are the primary targets (d near 1.0) — they bear the cost of denied autonomy with no compensating benefit and are trapped by network effects and switching costs. Dependent organizations are also targets (d high, ~0.85) — they are trapped by identity fusion (their workflows have become inseparable from proprietary platforms) and bear indirect costs. Future developers are targets (d moderate-high, ~0.75) — they are constrained by closed-source barriers but have some option to contribute to or adopt open-source alternatives. Users-as-rights-holders is the analytical beneficiary (d = 0.0) — they represent the vindicated principle that computing freedom is legitimate. Proprietary vendors are excluded (not seated) — their core interest (maintaining control) is structurally opposed to the reading. Regulatory authorities and the open source community are observers (d = 0.5) — they perceive the constraint but do not directly benefit from or bear its costs. The directionality derivation shows strong asymmetry: powerless individuals trapped in a global network bear extraction; organized institutional actors (vendors) are excluded; the principle itself is the beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The freedom imperative reading diagnoses the proprietary software constraint as a case where the founding problem (developer revenue, copyright protection) has outlived its justification. This reading asserts that in a context of digital dependency and global computing infrastructure, user autonomy is the foundational need, not vendor profit protection. The constraint's mandate (intellectual property protection) is invoked to defend a restriction (user freedom denial) whose necessity is not self-evident. From this reading's perspective, the constraint exhibits mandatrophy: it persists because it is enforced legally and technically, not because it solves a live coordinating problem. However, the pragmatic openness reading and property rights reading would dispute this mandatrophy diagnosis — they would argue that the founding problem (incentivizing software development) is still live and that property rights are the legitimate mechanism. The mandatrophy claim is reading-indexed: it depends on accepting the freedom imperative as the governing value. The engine's per-seat classification will show whether seats with different exit options and power asymmetries converge on or diverge from the mandatrophy diagnosis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    freedom_definition_ambiguity,
    'What constitutes ''control'' in software control? Is it absolute modification rights, or does auditability + transparency + the right to fork + user-facing configurability count as sufficient control?',
    'Examine whether users'' computing autonomy is meaningfully expanded by each of these forms of control. Survey users in open-source environments to measure perceived autonomy and sense of control.',
    'If strict modification rights are required, proprietary software with open interfaces and auditability remains extractive. If auditability + forkability suffices, some proprietary-like arrangements might satisfy the freedom imperative. If user configurability suffices, many current systems approach compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(freedom_definition_ambiguity, conceptual, 'Whether user computing autonomy requires modification rights or can be satisfied by transparency and accountability.').

omega_variable(
    vendor_innovation_necessity,
    'Is proprietary software''s closed control model necessary for the pace and scale of software innovation? Does open-source development produce equivalent or superior innovation outcomes?',
    'Comparative analysis of feature development speed, security patching, adoption rates, and breakthrough innovation across proprietary and open-source software categories. Natural experiments from sectors transitioning to open-source infrastructure.',
    'If proprietary control is necessary for innovation, the freedom imperative reading must address whether sacrificing autonomy for innovation is a legitimate tradeoff. If open-source equals or exceeds proprietary innovation, the freedom imperative''s case strengthens substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_innovation_necessity, empirical, 'Whether proprietary control is empirically necessary for software innovation.').

omega_variable(
    collective_action_problem_irreducibility,
    'Is software infrastructure genuinely a collective-action problem that requires centralized proprietary coordination, or is it a technology where decentralized open-source governance is structurally superior?',
    'Examine whether large-scale infrastructure (operating systems, cloud platforms, databases) can be managed effectively as commons without centralized proprietary vendor control. Study historical transitions (Linux adoption, cloud-native open-source, etc.).',
    'If decentralized governance is viable, proprietary software appears as choice rather than necessity, and the freedom imperative''s framing of proprietary software as illegitimate coercion strengthens. If centralization is structurally necessary, the reading must engage with whether user freedom can be balanced against coordination necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_action_problem_irreducibility, empirical, 'Whether software infrastructure requires proprietary centralization or permits open-source governance.').

omega_variable(
    reading_foreclosure_on_property_rights,
    'Does the freedom imperative reading''s assertion that computing autonomy is fundamental logically foreclose the property rights reading (which asserts creators'' legitimate authority to restrict use), or is the foreclosure only apparent and both remain theoretically tenable in different value systems?',
    'Philosophical analysis: can a framework simultaneously hold that users have fundamental computing rights AND creators have fundamental property rights, or do these axioms contradict? Examine whether the contradiction is logical (one denies the other''s core premise) or merely practical (both cannot be maximized simultaneously but can be negotiated).',
    'If foreclosure is genuine, the engine''s signature detection should mark the property rights reading as logically ruled out by this reading''s axioms. If both remain tenable, they coexist and the foreclosure routing does not apply.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_on_property_rights, conceptual, 'Whether the freedom imperative axioms logically foreclose the property rights reading.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression of user autonomy primarily structural (legal barriers, technical lock-in, network effects that persist external to individual choice) or partially internalized (users have been convinced that proprietary software is natural/inevitable, that closed systems are trustworthy, that they deserve no control)?',
    'Post-transition analysis: where proprietary systems have been fully replaced by open-source alternatives (Linux in servers, LibreOffice ecosystems), does user autonomy expand immediately or are there persistent cognitive patterns where users still expect closed-source systems? Survey developers and users migrating from proprietary to open ecosystems.',
    'If suppression is primarily structural, removing the constraint (mandating open source) should rapidly restore autonomy. If partially internalized, users may need re-education and resocialization even after structural barriers fall, suggesting the constraint''s cultural depth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of user autonomy is structural or partially internalized.').

omega_variable(
    sibling_reading_contest_empirical_ground,
    'Is the disagreement between this reading (freedom imperative) and the property rights reading fundamentally empirical (about whether proprietary control is necessary), normative (about whether property rights override computing rights), or both?',
    'Decompose the disagreement: isolate claims about facts (does closed-source development produce better software?) from claims about values (should users'' autonomy override creators'' property claims?). Separate the empirical questions (resolvable by data) from the normative ones (requiring value judgments).',
    'If the disagreement is purely normative, data cannot resolve it — regulatory intervention would require explicit value choice. If empirical questions are entangled, resolving the empirical facts may shift the normative calculus. This affects which reading-resolution pathway is available.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_contest_empirical_ground, conceptual, 'The empirical vs. normative structure of disagreement between freedom imperative and property rights readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__freedom_imperative_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(soft_tr_t0, observed).
narrative_ontology:measurement(soft_tr_t5, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(soft_tr_t5, observed).
narrative_ontology:measurement(soft_tr_t10, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement_basis(soft_tr_t10, observed).
narrative_ontology:measurement(soft_tr_t15, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(soft_tr_t15, observed).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(soft_tr_t20, observed).
narrative_ontology:measurement(soft_tr_t25, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(soft_tr_t25, observed).
narrative_ontology:measurement(soft_tr_t30, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(soft_tr_t30, observed).
narrative_ontology:measurement(soft_tr_t40, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(soft_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 0, 0.82).
narrative_ontology:measurement_basis(soft_be_t0, observed).
narrative_ontology:measurement(soft_be_t5, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 5, 0.84).
narrative_ontology:measurement_basis(soft_be_t5, observed).
narrative_ontology:measurement(soft_be_t10, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 10, 0.85).
narrative_ontology:measurement_basis(soft_be_t10, observed).
narrative_ontology:measurement(soft_be_t15, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 15, 0.87).
narrative_ontology:measurement_basis(soft_be_t15, observed).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 20, 0.88).
narrative_ontology:measurement_basis(soft_be_t20, observed).
narrative_ontology:measurement(soft_be_t25, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 25, 0.88).
narrative_ontology:measurement_basis(soft_be_t25, observed).
narrative_ontology:measurement(soft_be_t30, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 30, 0.89).
narrative_ontology:measurement_basis(soft_be_t30, observed).
narrative_ontology:measurement(soft_be_t40, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 40, 0.89).
narrative_ontology:measurement_basis(soft_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(soft_su_t0, observed).
narrative_ontology:measurement(soft_su_t5, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 5, 0.69).
narrative_ontology:measurement_basis(soft_su_t5, observed).
narrative_ontology:measurement(soft_su_t10, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement_basis(soft_su_t10, observed).
narrative_ontology:measurement(soft_su_t15, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement_basis(soft_su_t15, observed).
narrative_ontology:measurement(soft_su_t20, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement_basis(soft_su_t20, observed).
narrative_ontology:measurement(soft_su_t25, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement_basis(soft_su_t25, observed).
narrative_ontology:measurement(soft_su_t30, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 30, 0.77).
narrative_ontology:measurement_basis(soft_su_t30, observed).
narrative_ontology:measurement(soft_su_t40, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement_basis(soft_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__freedom_imperative_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__freedom_imperative_reading, 0.12).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__commons_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, digital_infrastructure_dependency).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, vendor_lock_in_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the kernel 'software_control_legitimacy'. The freedom imperative reading treats software control as a matter of human rights and deems proprietary software categorically extractive. The sibling readings (pragmatic_openness, property_rights, commons) decompose the same kernel into structurally distinct constraints with different ε values, beneficiary/victim structures, and classifications. Each reading authors its own constraint story independently; they are linked via network.affects_constraints as a constraint family. The decomposition follows the ε-invariance principle: each reading instantiates a different constraint because the referent (standing arrangement under contest) and its measurement (extractiveness, suppression) are reading-indexed. Authoring them as one story with multiple measurement bases would violate the principle; decomposing them enables the corpus to measure which readings' classifications diverge and by how much.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
