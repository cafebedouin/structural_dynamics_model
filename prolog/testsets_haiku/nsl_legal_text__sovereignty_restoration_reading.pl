% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__sovereignty_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__sovereignty_restoration_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: nsl_legal_text__sovereignty_restoration_reading
 *   human_readable: National Security Law as Sovereignty Restoration (Constitutional Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   On June 30, 2020, the Hong Kong National Security Law was enacted in
 *   response to escalating street protests and institutional contestation in
 *   2019. This story models NSL as it is framed in sovereignty-restoration
 *   doctrine: a legitimate exercise of central government prerogative to
 *   re-establish constitutional order and suppress subversion after a period
 *   of institutional breakdown. Protesters and opposition voices are reframed
 *   as security threats; the central authority gains benefit through restored
 *   control; the extraction is moderate but active. This is ONE READING of a
 *   contested kernel (nsl_legal_text). Sibling readings
 *   (democratic_enclosure_reading, jurisdictional_capture_reading) model
 *   alternative framings of the same legal text. The sovereignty-restoration
 *   reading is the framing adopted by Beijing, the Hong Kong executive, and
 *   international allies; the alternative readings are held by activists,
 *   common-law jurists, and international critics. This story does NOT
 *   adjudicate between them—it models THIS reading as a coherent constraint
 *   structure. The claim (tangled_rope) and the metrics (moderate
 *   extractiveness, high suppression) are authored independently: the
 *   sovereignty-restoration framing presents the law as coordination
 *   (restoring order) that happens to require enforcement against those who
 *   reject its legitimacy premise.
 *
 * KEY AGENTS:
 *   - central_government_authority: Installs and retains veto over NSL; sole beneficiary of restored sovereign control (institutional, arbitrage exit, global scope)
 *   - hong_kong_administrative_apparatus: Executes NSL enforcement; benefits from restored executive prerogative; constrained by dependency on mainland authorization (institutional, trapped exit, regional scope)
 *   - protest_activists: Criminalized under NSL; identity-locked through political commitment; dispersed and surveilled (powerless, identity-locked exit, regional scope)
 *   - pro_democracy_legislators: Face prosecution for lawful prior advocacy; lose institutional platform (moderate power, constrained exit, regional scope)
 *   - independent_journalists: Self-censor under liability; retain geographic exit but at cost of professional abandonment (moderate power, constrained exit, regional scope)
 *   - business_and_capital_interests: Benefit from restored order; retain mobility; minimal direct suppression (powerful, mobile exit, regional scope)
 *   - common_law_judicial_tradition: Structurally excluded from the law's authorization frame; cannot adjudicate its legitimacy (non-agent, powerful, trapped exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, 0.56).
domain_priors:suppression_score(nsl_legal_text__sovereignty_restoration_reading, 0.68).
domain_priors:theater_ratio(nsl_legal_text__sovereignty_restoration_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__sovereignty_restoration_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__sovereignty_restoration_reading, "National Security Law as Sovereignty Restoration (Constitutional Reading)").
narrative_ontology:topic_domain(nsl_legal_text__sovereignty_restoration_reading, "constitutional/political").

domain_priors:requires_active_enforcement(nsl_legal_text__sovereignty_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__sovereignty_restoration_reading, 'eae03c06-fd3f-47c4-8123-d61ee8cecad4').
narrative_ontology:cs_kernel_codification('eae03c06-fd3f-47c4-8123-d61ee8cecad4', formalized).
narrative_ontology:cs_authority_grounding('eae03c06-fd3f-47c4-8123-d61ee8cecad4', extraction).
narrative_ontology:cs_reading_relation('eae03c06-fd3f-47c4-8123-d61ee8cecad4', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('eae03c06-fd3f-47c4-8123-d61ee8cecad4', nsl_legal_text__jurisdictional_capture_reading, influences).
narrative_ontology:cs_axiom('eae03c06-fd3f-47c4-8123-d61ee8cecad4', foundational, legitimate_sovereign_prerogative_in_constitutional_crisis).
narrative_ontology:cs_axiom_status(legitimate_sovereign_prerogative_in_constitutional_crisis, holdable).
narrative_ontology:cs_axiom_grounding('eae03c06-fd3f-47c4-8123-d61ee8cecad4', legitimate_sovereign_prerogative_in_constitutional_crisis, deontological).
narrative_ontology:cs_axiom('eae03c06-fd3f-47c4-8123-d61ee8cecad4', foundational, security_threat_justifies_temporary_rights_restriction).
narrative_ontology:cs_axiom_status(security_threat_justifies_temporary_rights_restriction, holdable).
narrative_ontology:cs_axiom_grounding('eae03c06-fd3f-47c4-8123-d61ee8cecad4', security_threat_justifies_temporary_rights_restriction, empirically_contingent).
narrative_ontology:cs_reference_frame('eae03c06-fd3f-47c4-8123-d61ee8cecad4', sovereign_constitutional_prerogative).
narrative_ontology:cs_drift_state('eae03c06-fd3f-47c4-8123-d61ee8cecad4', contemporary_human_rights_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('eae03c06-fd3f-47c4-8123-d61ee8cecad4', '2026-06-12T14:32:51Z').
narrative_ontology:cs_kernel_id(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, central_government_authority).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, hong_kong_administrative_apparatus).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, protest_activists).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, pro_democracy_legislators).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, independent_journalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, business_and_capital_interests).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, hong_kong_administrative_apparatus).
narrative_ontology:constraint_vindicates(nsl_legal_text__sovereignty_restoration_reading, sovereignty_indivisibility_doctrine).
narrative_ontology:constraint_vindicates(nsl_legal_text__sovereignty_restoration_reading, administrative_prerogative_in_crisis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and enacts NSL in response to 2019 street unrest that challenged Hong Kong's constitutional subordination. Frames the law as restoration of sovereign prerogative and constitutional order after a period of institutional breakdown. Maintains primary enforcement authority over national security matters and retains veto over judicial interpretation. Benefits from restored political control over an ungoverned institutional space.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, central_government_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% The Hong Kong government receives restored executive authority to suppress sustained protest and criminalize political opposition framed as subversion. Simultaneously bound by the law it did not author and constrained by the judicial review norms it nominally operates under. Benefits from restored order and reduced street pressure; constrained by reduced institutional autonomy from the mainland authority.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, hong_kong_administrative_apparatus, beneficiary,
    institutional, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__sovereignty_restoration_reading, hong_kong_administrative_apparatus, payer).

% Face criminal liability for acts of dissent previously shielded by judicial construction of the Basic Law. Identity-locked through political commitment and relational embeddedness in protest community; exit means abandoning political self-conception. Dispersed resistance impossible to coordinate under surveillance and legal threat; capture mechanisms make continued activism prosecutable.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, protest_activists, payer,
    powerless, biographical, identity_locked, regional).

% Lose institutional platform and face potential prosecution for legislative advocacy. Constrained exit: remain and risk prosecution, resign and forfeit any voice, or flee and abandon constituents. The law retroactively criminalizes prior lawful speech, collapsing the alternative of legitimate opposition within the system.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, pro_democracy_legislators, payer,
    moderate, biographical, constrained, regional).

% Self-censor under liability for reporting on national security matters. Definition of national security is expansive and administered by the executive without transparent criteria. Exit options: cease reporting on sensitive topics, leave the jurisdiction, or accept prosecution risk. Many choose geographic exit; those remaining operate under sustained chilling effect.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, independent_journalists, payer,
    moderate, biographical, constrained, regional).

% Benefit from restored order and suppression of street disruption that interrupted commerce and deterred investment. Maintain legal immunity if politically aligned or silent; retain geographic mobility if costs rise. Minimal direct suppression; primary gain is political stability enabling economic activity.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, business_and_capital_interests, beneficiary,
    powerful, generational, mobile, regional).

% Not a seat, but a normative structure excluded from the law's authorization frame. Common law adjudication traditions (precedent, judicial independence, proportionality review) are overridden by statutory command and executive prerogative. A doctrine that might object to the law's retroactivity and overbreadth is structurally barred from speaking.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, common_law_judicial_tradition, excluded,
    powerful, generational, trapped, regional).
narrative_ontology:stakeholder_non_agent(nsl_legal_text__sovereignty_restoration_reading, common_law_judicial_tradition).

% Document enforcement patterns, arrest trends, and prosecutorial scope. Provide external measurement of suppression intensity and targeting. Cannot enforce remedies but generate the factual record against which the sovereignty-restoration framing is tested.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, international_human_rights_monitors, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__sovereignty_restoration_reading, central_government_authority).
narrative_ontology:fixing_cost_class(nsl_legal_text__sovereignty_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a centralized security authority capable of suppressing coordinated mass protest and institutional breakdown. Solves the coordination problem of state response to sustained civil unrest by unifying security doctrine and enforcement across what were previously contested institutional domains (courts, legislature, police).
% TRANSFER_FUNCTION: Transfers political agency and speech rights from civil society actors (protesters, opposition legislators, independent media) to the central security authority and its designates. Moves the cost of dissent from negligible (protected speech) to high (criminal liability and prosecution).
% ABSENT_VOICES: Voices advocating for common law continuity, judicial independence, or proportionality constraints are structurally excluded—the law is administered by the authority it empowers, not subject to the independent review those voices would demand. International human rights monitors can document but not reshape the constraint. Mainland legal professionals inside the system cannot publicly contest the law's authority without career jeopardy.
% DISAPPEARANCE_RATIONALE: If NSL vanished, Hong Kong's institutional setup would revert to the pre-2019 state: courts would restore prior common law protections, protest would resume at prior levels, and the executive's security prerogative would shrink to prior scope. A significant constituency (central government, capital interests) would lose material benefit; activists and opposition would regain speech and assembly space. The world does rearrange.
% FOUNDING_PROBLEM: Sustained mass street unrest in 2019 that paralyzed commerce, challenged institutional authority, and created a coordination crisis: the Hong Kong executive could not restore order through ordinary law, courts resisted expansive security interpretations, and the unrest was experienced as a legitimacy crisis by Beijing. The law was built to solve this specific coordination failure.
% FOUNDING_PROBLEM_CORROBORATION: Central government officials and Hong Kong business representatives attest the founding problem was acute and required the law for resolution. Protest organizers and international observers attest the problem was a period of heightened confrontation that de-escalated through its own trajectory and enforcement responses—that the founding problem was resolving without legal innovation before NSL was drafted. No independent third party (aside from those with security or commercial benefit) corroborates that legal codification was structurally necessary for order restoration.
narrative_ontology:disappearance_verdict(nsl_legal_text__sovereignty_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__sovereignty_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__sovereignty_restoration_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(nsl_legal_text__sovereignty_restoration_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 at enactment to 0.56 by year 5, reflecting expanding scope of prosecutions and widening interpretation of national security crimes. Suppression rises sharply (0.42→0.68 by year 5) as enforcement infrastructure matures—surveillance systems, task forces, and interrogation procedures become routinized. Theater rises from 0.18 to 0.43 over the interval: security-restoration rhetoric initially dominates (low theater), but by year 5 the gap between stated coordination purpose (restoration of order, which succeeds) and actual enforcement pattern (targeting political opposition, which is the primary measure of enforcement success) creates performative elements. The trajectory shows suppression hardening faster than extractiveness rises, suggesting the enforcement machinery is built before the extraction logic fully stabilizes. All metrics authored at every shared time point (the alignment rule): this is one measurement grid for all three series.
 *
 * PERSPECTIVAL GAP:
 *   From the central authority's seat, NSL is legitimate sovereignty restoration: it solves a real coordination problem (2019 unrest) and the exercise of prerogative is lawful under the constitutional order. From the activist and opposition seats, the same legal text operates as permanent criminalization of political activity under a retroactive and expansive security rationale. The engine computes these as different seats' different directionalities: central authority near d=0.0 (beneficiary, arbitrage exit), activists near d=1.0 (targets, identity-locked exit). The authored metrics do NOT reconcile these perspectives—they describe the sovereignty-restoration reading's internal consistency. The claim of tangled_rope (genuine coordination need + asymmetric extraction) reflects the reading's own logic: the coordination is real (order does stabilize), the extraction is real (opposition loses rights), and enforcement is real (the law is actively administered). Whether this is a legitimate balancing or illegitimate oppression is the contested question the kernel holds open.
 *
 * DIRECTIONALITY LOGIC:
 *   Central government: acts as beneficiary and agenda-setter; gains control over security determination; retains arbitrage exit (can escalate or withdraw enforcement, can reinterpret the law). Directionality near 0.0 (beneficiary, unharmed by extraction). Hong Kong administrative apparatus: sits between beneficiary (gains executive authority) and payer (loses institutional autonomy to mainland veto). Directionality near 0.3-0.4 (weak beneficiary, constrained by dependency). Activists and opposition: face criminalized status, surveillance, and prosecutorial exposure; identity-locked through political commitment (exit means self-negation); constrained by prior law's own protection collapse. Directionality near 0.8-0.9 (high target, trapped/identity-locked exit). Business interests: benefit from order restoration; mobile (can shift investment if costs rise); minimal direct suppression. Directionality near 0.1-0.2 (beneficiary, mobile exit). The derivation chain: beneficiary/victim declarations + exit_options + power_atom → directionality values the engine computes. No overrides needed; the structural data produces the seated asymmetry directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereignty-restoration reading preserves its founding problem as live: unrest still requires continued suppression; Beijing must maintain security prerogative to sustain order; the coordinate constraint (restoring authority) is inseparable from the extractive mechanism (criminalizing opposition). Mandatrophy would arise if suppression succeeded so completely that opposition vanished entirely—then the founding problem would be dead and the constraint would persist only as inertia (piton). Current state (year 5): suppression is high and harassment is constant, but resistance persists (0.74 authored resistance), suggesting the founding problem remains live in this reading's logic. The tangled_rope classification reflects this: genuine coordination need (stable order) + active extraction (targeted prosecution) + continued enforcement (rising suppression requirement), not yet atrophied into pure performance. The reading forestalls mandatrophy by maintaining that opposition persistence is itself evidence that the security threat is still live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_legitimacy_boundary,
    'Does the existence of sovereign constitutional power (the right to act) establish the legitimacy of its exercise (whether the action respects procedural and substantive constraints on that power)?',
    'Judicial review under a principle of proportionality or margin-of-appreciation doctrine (if courts retain independence); comparative constitutional law analysis of how other sovereigns constrain security prerogatives; post-NSL jurisprudence asking whether courts enforce limits on security-law interpretation.',
    'If sovereignty and legitimacy are separable, NSL may be constitutional-as-authorized but illegitimate-as-administered, collapsing the framing back into democratic-enclosure territory; if identity, the sovereignty-restoration reading holds intact. This is the crux.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_legitimacy_boundary, conceptual, 'Whether sovereign power to enact NSL entails legitimacy of its exercise under the NSL''s own terms or under external constitutional limits.').

omega_variable(
    unrest_causation_vs_necessity,
    'Was the 2019 unrest caused by (a) genuine grievances about governance legitimacy and democratic representation, or (b) hostile foreign interference and subversive conspiracy? Did NSL address the cause or suppress the symptom?',
    'Comparative analysis of protest movements in similar democracies; testimony from protest organizers about motivations; intelligence analysis of foreign involvement; post-NSL trajectory of unrest (does it continue under suppression, suggesting underlying cause persists, or vanish, suggesting coercive success).',
    'If (a), NSL bypasses the founding problem rather than solving it and is at best symptomatic treatment of institutional delegitimacy; if (b), NSL directly addresses a security threat and the framing holds. Current evidence shows continued protest under suppression, suggesting (a), but the reading maintains (b) by interpreting continuation as evidence of ongoing threat rather than residual grievance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unrest_causation_vs_necessity, empirical, 'Root cause of 2019 unrest: governance grievance vs. subversive conspiracy.').

omega_variable(
    extraction_asymmetry_and_targeting,
    'Is NSL enforcement symmetric across security categories (applied equally to all alleged national-security threats regardless of political alignment) or asymmetric (concentrated on pro-democracy activism and opposition figures)?',
    'Prosecution statistics disaggregated by political alignment of accused; charging decisions for comparable conduct across aligned/opposition actors; documented non-enforcement against aligned actors making similar speech.',
    'Asymmetric enforcement establishes NSL as political tool masquerading as security law (snare territory); symmetric enforcement supports the sovereignty-restoration reading by showing principled rather than targeted application. Available evidence shows asymmetry (pro-democracy activists prosecuted, aligned actors given immunity), but the reading maintains that asymmetry reflects legitimate differential dangerousness rather than political discrimination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_asymmetry_and_targeting, empirical, 'Enforcement asymmetry across political alignment: test of principled security law vs. political suppression.').

omega_variable(
    common_law_continuity_collapse,
    'Does NSL preserve common law procedural protections (retroactivity prohibition, proportionality review, judicial independence) or replace them with administrative discretion and executive override?',
    'Textual analysis of NSL''s carveouts and limitations; judicial decisions testing proportionality and retroactivity challenges; documented instances of court decisions reversed or ignored by executive authority.',
    'If common law protections survive NSL, the constraint models tangled-rope—coordination + enforcement within a bounded institutional framework. If they collapse, NSL operates as pure executive prerogative (snare) with courts as ceremonial rubber stamps. The reading maintains the former; evidence shows the latter emerging, triggering the democratic-enclosure reading''s logic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(common_law_continuity_collapse, empirical, 'Whether NSL preserves or dismantles common law procedural constraints on state power.').

omega_variable(
    reading_foreclosure_on_sovereignty_axiom,
    'Do the sibling readings (democratic-enclosure, jurisdictional-capture) foreclose or coexist with the sovereignty-restoration reading on the foundational axiom of ''legitimate sovereign prerogative in constitutional crisis''?',
    'Definitional: if a party accepts the axiom, can they coherently hold the sibling reading? Democratic-enclosure denies the axiom (prerogative is pretextual); jurisdictional-capture is agnostic (prerogative might be legitimate but colonizes local autonomy). Reading-level logical structure, not empirical.',
    'If democratic-enclosure forecloses sovereignty-restoration, the readings are in binary opposition and the kernel resolves into one of two states. If they coexist, the kernel is persistently contested by different institutional actors, each maintaining its reading. Current state: coexist (different seats hold different readings), suggesting no foreclosure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_on_sovereignty_axiom, conceptual, 'Whether sovereignty-restoration and democratic-enclosure readings foreclose each other or coexist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__sovereignty_restoration_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(nsl__tr_t0, observed).
narrative_ontology:measurement(nsl__tr_t2, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2, 0.24).
narrative_ontology:measurement_basis(nsl__tr_t2, observed).
narrative_ontology:measurement(nsl__tr_t5, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement_basis(nsl__tr_t5, observed).
narrative_ontology:measurement(nsl__tr_t10, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement_basis(nsl__tr_t10, observed).
narrative_ontology:measurement(nsl__tr_t15, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 15, 0.43).
narrative_ontology:measurement_basis(nsl__tr_t15, observed).
narrative_ontology:measurement(nsl__tr_t20, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(nsl__tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(nsl__be_t0, observed).
narrative_ontology:measurement(nsl__be_t2, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2, 0.45).
narrative_ontology:measurement_basis(nsl__be_t2, observed).
narrative_ontology:measurement(nsl__be_t5, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(nsl__be_t5, observed).
narrative_ontology:measurement(nsl__be_t10, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(nsl__be_t10, observed).
narrative_ontology:measurement(nsl__be_t15, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement_basis(nsl__be_t15, observed).
narrative_ontology:measurement(nsl__be_t20, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement_basis(nsl__be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(nsl__su_t0, observed).
narrative_ontology:measurement(nsl__su_t2, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2, 0.55).
narrative_ontology:measurement_basis(nsl__su_t2, observed).
narrative_ontology:measurement(nsl__su_t5, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement_basis(nsl__su_t5, observed).
narrative_ontology:measurement(nsl__su_t10, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(nsl__su_t10, observed).
narrative_ontology:measurement(nsl__su_t15, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(nsl__su_t15, observed).
narrative_ontology:measurement(nsl__su_t20, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(nsl__su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__sovereignty_restoration_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nsl_legal_text__sovereignty_restoration_reading, 0.18).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__democratic_enclosure_reading).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__jurisdictional_capture_reading).

% DUAL FORMULATION NOTE:
% The NSL_legal_text kernel generates three structurally distinct constraint stories, each with its own ε, beneficiary/victim structure, and empirical classification. This story (sovereignty_restoration_reading) models NSL as legitimate constitutional restoration—moderate extraction targeting security threats. The democratic_enclosure_reading models the same legal text as permanent suppression hiding behind security rationale—high extraction, all-population suppression. The jurisdictional_capture_reading models NSL as mainland legal transplantation eroding Hong Kong autonomy. These are not different measurements of the same constraint; they are different constraint readings of a shared kernel (the NSL statutory text). Each reading instantiates different beneficiary/victim sets (this reading: central authority benefits, activists victimized; democratic_enclosure: regime benefits, civil society victimized broadly; jurisdictional_capture: mainland authority benefits, local legal tradition victimized). Differ by measured ε: sovereignty_restoration 0.56, democratic_enclosure ~0.75, jurisdictional_capture ~0.63. All three are linked via network.affects_constraints to show kernel relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
