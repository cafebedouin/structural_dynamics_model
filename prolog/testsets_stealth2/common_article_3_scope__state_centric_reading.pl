% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__state_centric_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: common_article_3_scope__state_centric_reading
 *   human_readable: Common Article 3 Scope — State-Centric Threshold Reading
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions promises a minimum
 *   humanitarian floor — humane treatment, and prohibition of murder, cruel
 *   treatment, hostage-taking, and unfair trial — for persons in internal
 *   armed conflict. The kernel under contest is the article's scope: what
 *   counts as the 'armed conflict not of an international character' that
 *   switches the floor on. This story instantiates the state-centric reading:
 *   the floor applies only where violence crosses intensity and organization
 *   thresholds, low-level violence and law-enforcement operations stay
 *   outside it, and the territorial government is the operative classifier.
 *   The standing arrangement under contest — the threshold-gated arrangement
 *   — is the ε referent, assessed by this reading's own lights: a legitimate
 *   line-drawing that preserves regime coherence and state consent, whose
 *   costs (persons in below-threshold violence left outside the floor) the
 *   reading justifies rather than denies. The sibling readings —
 *   expansive_human_rights_reading (the floor attaches to any organized armed
 *   violence) and icrc_customary_reading (scope tracks evolving state
 *   practice and opinio juris) — are separate constraint stories over the
 *   same kernel with their own ε and victim sets; they are not folded into
 *   this one (one reading, one constraint, one ε). The claim/metric gap is
 *   deliberate: the reading is CLAIMED as tangled_rope while the metrics
 *   describe its actual operation — the engine measures per-seat divergence
 *   from the structural data; the claim is not tuned to a predicted output.
 *
 * KEY AGENTS:
 *   - territorial_state_governments: Primary beneficiary and agenda setter (institutional/arbitrage) — controls classification, collects the discretion, funds the interpretive machinery
 *   - state_legal_advisors: In-frame interpreter (moderate/identity_locked) — produces the determinations, professionally fused with the reading
 *   - national_prosecution_services: Secondary beneficiary (institutional/constrained) — collects full domestic jurisdiction below threshold
 *   - sub_threshold_irregular_fighters: Primary target (powerless/trapped) — bears the arrangement's costs directly
 *   - sub_threshold_conflict_civilians: Primary target (powerless/trapped) — left outside the humanitarian floor
 *   - nonstate_armed_groups: Dual-positioned party (organized/constrained) — members pay while the group escapes treaty obligations
 *   - icrc_delegates: Structurally excluded monitor (organized/constrained) — treaty access right is threshold-gated
 *   - human_rights_treaty_bodies: Analytical observer (institutional/analytical) — monitors under the human-rights overlay the reading concedes
 *   - domestic_courts: Enforcement interpreter (institutional/constrained) — legitimates classifications while occasionally narrowing the frame from inside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, 0.46).
domain_priors:suppression_score(common_article_3_scope__state_centric_reading, 0.7).
domain_priors:theater_ratio(common_article_3_scope__state_centric_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__state_centric_reading, "Common Article 3 Scope — State-Centric Threshold Reading").
narrative_ontology:topic_domain(common_article_3_scope__state_centric_reading, "legal/international_humanitarian_law").

domain_priors:requires_active_enforcement(common_article_3_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__state_centric_reading, '2c493474-9045-4a6e-ae65-3ede3e9e292d').
narrative_ontology:cs_kernel_codification('2c493474-9045-4a6e-ae65-3ede3e9e292d', fixed_text).
narrative_ontology:cs_authority_grounding('2c493474-9045-4a6e-ae65-3ede3e9e292d', extraction).
narrative_ontology:cs_interpretation_layer_present('2c493474-9045-4a6e-ae65-3ede3e9e292d').
narrative_ontology:cs_reading_relation('2c493474-9045-4a6e-ae65-3ede3e9e292d', common_article_3_scope__expansive_human_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('2c493474-9045-4a6e-ae65-3ede3e9e292d', common_article_3_scope__icrc_customary_reading, coexists_with).
narrative_ontology:cs_axiom('2c493474-9045-4a6e-ae65-3ede3e9e292d', foundational, threshold_gated_treaty_scope).
narrative_ontology:cs_axiom_status(threshold_gated_treaty_scope, holdable).
narrative_ontology:cs_axiom_grounding('2c493474-9045-4a6e-ae65-3ede3e9e292d', threshold_gated_treaty_scope, conventional).
narrative_ontology:cs_axiom('2c493474-9045-4a6e-ae65-3ede3e9e292d', foundational, state_consent_fixes_obligation_scope).
narrative_ontology:cs_axiom_status(state_consent_fixes_obligation_scope, holdable).
narrative_ontology:cs_axiom_grounding('2c493474-9045-4a6e-ae65-3ede3e9e292d', state_consent_fixes_obligation_scope, conventional).
narrative_ontology:cs_reference_frame('2c493474-9045-4a6e-ae65-3ede3e9e292d', geneva_1949_threshold_compromise).
narrative_ontology:cs_drift_state('2c493474-9045-4a6e-ae65-3ede3e9e292d', contemporary_classification_contest_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2c493474-9045-4a6e-ae65-3ede3e9e292d', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__state_centric_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, territorial_state_governments).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, national_prosecution_services).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, sub_threshold_irregular_fighters).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, sub_threshold_conflict_civilians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, nonstate_armed_groups).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, nonstate_armed_groups).
narrative_ontology:constraint_vindicates(common_article_3_scope__state_centric_reading, consent_bound_treaty_obligations).
narrative_ontology:constraint_vindicates(common_article_3_scope__state_centric_reading, war_crime_law_enforcement_distinction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determine whether violence on their territory counts as an armed conflict meeting the intensity and organization thresholds, and therefore which legal regime governs their operations. Below the threshold they run detention, prosecution, and force decisions under their own criminal law and human-rights obligations, with no treaty-based humanitarian floor and no required access for outside monitors. They can move between the law-enforcement frame and the conflict frame as their operational interests shift, and they fund and staff the interpretive machinery that produces the classification determinations.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, territorial_state_governments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__state_centric_reading, territorial_state_governments, beneficiary).

% Government and military lawyers who draft the classification determinations and advise on what the thresholds permit. Careers and professional standing are built inside the state-centric reading; advisers who questioned threshold positions in past administrations often found themselves sidelined, so the advising corps has strong reasons to keep producing determinations within the frame they were trained in. Leaving the frame would mean repudiating their own past advice and their professional community.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, state_legal_advisors, agenda_setter,
    moderate, biographical, identity_locked, national).

% Retain full domestic jurisdiction over members of armed groups when violence is classified below the threshold: charges, procedure, sentencing, and detention all run through ordinary criminal law the state controls. A conflict classification would import humanitarian-law constraints on detention and trial and give defense counsel additional arguments, so prosecutors have an institutional stake in the low-threshold outcome.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, national_prosecution_services, beneficiary,
    institutional, biographical, constrained, national).

% Members of armed groups in violence their government classifies below the threshold. They receive no combatant privileges, no humanitarian-floor detention protections, and no third-party access when held; they face prosecution under domestic law for participation itself, and the classification decision that governs their treatment is made entirely by the adversary they fight. Leaving the situation would mean leaving the armed group and often the country.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, sub_threshold_irregular_fighters, payer,
    powerless, immediate, trapped, national).

% Civilians living where violence falls below the classification threshold. The treaty floor that forbids murder, cruel treatment, hostage-taking, and unfair trial in armed conflicts does not attach to the violence around them; their protection runs through domestic law and human-rights mechanisms they rarely have the standing or resources to invoke, and displacement or exit is usually not available to them.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, sub_threshold_conflict_civilians, payer,
    powerless, biographical, trapped, local).

% Armed groups fighting governments that classify the violence below the threshold. The treaty floor neither binds nor protects their members, so their fighters face domestic prosecution and their own detainees fall outside humanitarian regulation; at the same time, the group itself is not subject to the treaty's obligations and can reject outside monitoring as a matter of law. Their members' protection depends on the group's own discipline and on whatever human-rights law the state's courts choose to apply.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, nonstate_armed_groups, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__state_centric_reading, nonstate_armed_groups, beneficiary).

% The neutral humanitarian organization holds a treaty-based right of initiative in common Article 3 conflicts, but below the classification threshold that right does not attach and access depends entirely on state consent — the same consent the threshold reading places in the government's discretion. The organization documents protection gaps in below-threshold violence and argues for broader application, but it can be kept out of precisely the situations the reading excludes.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, icrc_delegates, excluded,
    organized, generational, constrained, global).

% UN treaty bodies and regional human-rights courts monitor state conduct in below-threshold violence under human-rights law, which the reading concedes continues to apply. They publish findings on killings, detention, and trials in internal violence, keep the protection question on the record, and their jurisprudence narrows what the law-enforcement frame permits — but they cannot import the humanitarian floor itself.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, human_rights_treaty_bodies, observer,
    institutional, generational, analytical, global).

% National courts apply the intensity and organization tests when classification is contested in criminal cases, habeas petitions, and civil claims. Their threshold rulings bind future cases and give the classification determinations judicial legitimacy; they work inside the doctrinal frame the executive produces and rarely depart from it, though some constitutional courts have read human-rights protections into below-threshold operations.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, domestic_courts, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__state_centric_reading, territorial_state_governments).
narrative_ontology:fixing_cost_class(common_article_3_scope__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a workable boundary between the IHL regime and the domestic law-enforcement and human-rights regimes: the intensity and organization thresholds reserve the humanitarian floor for genuine armed conflict, keep riots, ordinary crime, and policing under criminal law, and preserve the consent-based architecture of treaty IHL in which states accept obligations by choice rather than by default.
% TRANSFER_FUNCTION: Moves classification discretion and operational latitude to territorial governments: below the threshold, detention, prosecution, and force decisions shift from the treaty's humanitarian floor to domestic criminal law the state itself controls, while the costs fall on fighters and civilians in below-threshold violence, who lose the minimum protections and third-party access the floor would provide.
% ABSENT_VOICES: Fighters and civilians in below-threshold conflicts have no seat in the classification decision that governs their treatment; the ICRC's treaty-based access right is threshold-gated, so its monitoring voice is structurally kept out of exactly the situations the reading excludes; human-rights bodies and the expansive-reading coalition contest from outside the frame but cannot import the floor themselves.
% DISAPPEARANCE_RATIONALE: If the threshold-gating vanished overnight and the humanitarian floor attached to all organized internal violence, detention regimes, prosecution practice, counterinsurgency doctrine, and ICRC access patterns would all rearrange: governments would lose classification discretion, thousands of prosecutions built on the law-enforcement frame would face humanitarian-law challenges, monitor access would become a right rather than a concession, and the war/crime distinction around which domestic security law is organized would have to be redrawn.
% FOUNDING_PROBLEM: The 1949 drafters needed a minimum protection floor for civil war without forcing states to grant belligerency status to insurgents — which would legitimize them — and without subjecting every internal disturbance, riot, and police operation to the full law of war; the common Article 3 threshold was the compromise that protected persons in genuine internal armed conflict while leaving lesser violence to domestic law.
% FOUNDING_PROBLEM_CORROBORATION: The 1949 drafting history (travaux préparatoires) and the ICRC's own contemporary commentary corroborate that the founding problem was real and is accurately stated. On status: the ICRC, UN human-rights mechanisms, and academic commentators outside the state beneficiary set attest that the problem has shifted — contemporary organized violence increasingly occupies the gap the 1949 line leaves — while state parties and their legal advisors attest that the original sovereignty-protective rationale remains live. The contest is corroborated from both sides by parties with different structural positions, not only by the arrangement's beneficiaries.
narrative_ontology:disappearance_verdict(common_article_3_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_article_3_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__state_centric_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__state_centric_reading_tests).
:- end_tests(common_article_3_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.46 and is reading-indexed: the same threshold-gated arrangement would draw a far higher ε from the expansive seat, but this story assesses the arrangement by the state-centric reading's own lights, which sees the threshold as legitimate line-drawing whose costs it explicitly justifies. The residual ε reflects what even this seat can see — a protection gap that widens as organized non-state violence proliferates below the 1949-era line, a widening the reading's own positivist scholars acknowledge as the 'gray zone' problem. Suppression (0.70) is a raw structural property, NOT scaled by power or scope — only extractiveness is scaled in the engine's computation: the arrangement persists only because governments actively maintain the classification boundary (denying conflict status, refusing monitor access, litigating thresholds), and the suppression_requirement series shows that coercive maintenance hardening across the interval. Theater (0.45) reflects classification exercises that are substantially outcome-driven — threshold analyses produced to reach predetermined low-threshold results — offset by genuinely structured judicial analysis in courts that take the tests seriously; the series dips at the Tadić codification (t46), which temporarily made threshold analysis more structured, then rises through the post-2001 classification contests. Accessibility collapse (0.50): the doctrinal alternatives (expansive and customary readings, the human-rights overlay) remain fully articulated and live, so nothing collapses at the doctrinal level, while for an individual detainee the alternative protection regime is controlled by the state itself. Resistance (0.65) is high and is largely the product of coalition rather than any single seat: the payer seats are individually powerless, but fighters' organizations, civilian networks, ICRC documentation, and human-rights litigation have repeatedly forced threshold questions into courts and treaty bodies.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats and the payer seats compute differently from the same structure. From the territorial government's seat the threshold is the price of a workable legal order: without it, every riot and police operation triggers war law, belligerency recognition legitimizes insurgents, and the consent-based treaty system dissolves. From the fighter's and the civilian's seat the same line is the difference between a protected person and a prosecutable subject — the floor's absence is experienced as abandonment to the adversary's domestic law. Inter-institutionally, the seats diverge by design of the reading itself: the ICRC's exclusion is the enforcement object, human-rights bodies are conceded a monitoring lane that cannot import the floor, and domestic courts legitimate the executive's classifications while occasionally narrowing the frame through human-rights reasoning. Among same-level actors, nominally equal sovereigns hold opposite threshold positions depending on whether their current conflicts are internal (high threshold preferred) or whether their adversaries' opponents are non-state (low threshold useful) — position tracks operational interest more than doctrine, which is the signal behind the good_faith_vs_motivated_classification omega. Identity-lock appears in the legal-advisor corps rather than the principals: advisers whose careers were built inside the reading have professional reasons to keep producing in-frame determinations even as the doctrinal ground shifts; if that professional identity frame broke, the interpretive layer that absorbs drift would thin and the reading's maintenance would depend entirely on explicit executive will.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map to real structural positions: territorial governments set and enforce the classification and collect the discretion (d near the beneficiary end, amplified by arbitrage-grade frame-switching between law-enforcement and conflict frames); prosecution services collect jurisdiction without running the arrangement. Fighters and civilians in below-threshold violence bear the arrangement's costs with no exit (d near the full-target end) — trapped, not identity-locked, since their position is imposed rather than fused. Non-state armed groups are genuinely dual-positioned — their members pay while the group escapes the treaty's obligations — so their derived directionality sits between the poles rather than at either end, captured via the secondary beneficiary role. The ICRC's position is structural exclusion rather than benefit or cost: the threshold removes its treaty access right exactly where it would matter most. No directionality overrides are used: the derivation from beneficiary/victim declarations plus power and exit atoms produces the right d for every seat, including the dual-positioned group through its secondary role, and overriding per power atom would misstate seats that share an atom but not a position (e.g., the ICRC and the armed groups are both 'organized').
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim keeps two facts in view that single-category labels would lose. Calling the threshold pure coordination would erase the protection gap — the persons the line leaves outside the floor are not a rounding error, and the arrangement's persistence depends on active enforcement rather than participant preference. Calling it pure extraction would erase the genuine coordination function: some violence-classification boundary is load-bearing for regime coherence, treaty consent, and the war/crime distinction, and the 1949 drafters' problem was real. The classification also guards against mandatrophy misreading in both directions: the founding problem is contested rather than dead — the parties dispute whether the 1949 line still tracks the conflicts that actually occur — so the arrangement is not an inertial relic administered by no one; and the gains are concentrated enough (named capturer, prohibitive cost-to-fix from the administrator's seat) that it is not a neglected vestige either. The mismatch consumer should read founding_problem_status=contested against the rising suppression_requirement series: the arrangement's enforcement is intensifying while its founding rationale is disputed — the signature of a live contest over a still-functioning structure, not of a mandate outlived.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is the state_centric_reading of kernel common_article_3_scope; the sibling readings (expansive_human_rights_reading, icrc_customary_reading) locate the disagreement in the classification predicate — the humanitarian floor attaches at the treaty threshold, at any organized armed violence, or wherever evolving custom draws the line — and each location produces a different victim set and a different ε over the same fixed referent (the threshold-gated arrangement).',
    'Comparative classification of the three sibling stories'' victim sets and ε values over the fixed referent; the disagreement is conceptual, resolved by which reading a framework adopts rather than by new data alone.',
    'If the expansive reading is adopted, sub_threshold_irregular_fighters and sub_threshold_conflict_civilians enter the victim set and ε rises sharply for the same arrangement; if the ICRC customary reading is adopted, the victim set tracks the custom-drawn line and ε drifts with state practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: one reading of the CA3-scope kernel; sibling readings restructure the victim set and ε.').

omega_variable(
    threshold_line_indeterminacy,
    'Where exactly do the intensity and organization thresholds sit, and therefore who is actually inside the excluded victim set?',
    'Systematic coding of state classification practice against the Tadić factors, plus litigation outcomes where courts must draw the line in contested cases.',
    'A lower line shrinks the excluded victim set and the measured extraction; a higher line grows both — the constraint''s ε is sensitive to the line''s placement, which the reading itself leaves indeterminate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_line_indeterminacy, empirical, 'Indeterminacy of the threshold''s empirical content and its effect on the victim set.').

omega_variable(
    good_faith_vs_motivated_classification,
    'Is the threshold maintained from good-faith positivist conviction about regime boundaries, or as motivated classification strategy that shields state operations from humanitarian-law scrutiny?',
    'Discordance analysis: compare states'' threshold positions across contexts where their operational interests flip — a state insisting on high thresholds for its own internal violence while invoking low thresholds against an adversary''s non-state allies would show interest-correlated rather than principled classification.',
    'If positions are systematically interest-correlated, the reading''s coordination function is largely cover and the constraint trends toward the snare end of the hybrid range; if positions track principle across interest flips, the coordination component of the tangled_rope classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(good_faith_vs_motivated_classification, empirical, 'Whether the threshold''s enforcement is principled or motivated classification.').

omega_variable(
    regime_boundary_necessity,
    'Is some violence-classification threshold a structural necessity of any coherent legal order, or a constructed choice whose current placement serves state interests?',
    'Counterfactual institutional design analysis: examine whether alternative boundary rules — graduated obligations, presumptive application with rebuttal, lower organization thresholds — would preserve regime coherence and the consent architecture at lower cost to persons in below-threshold violence.',
    'If a lower-cost boundary is coherent, the current threshold placement is a choice rather than a necessity, supporting the extraction component of the hybrid classification; if no alternative boundary is coherent, part of the measured cost is the price of legal order itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regime_boundary_necessity, conceptual, 'Whether the threshold is a structural necessity or a constructed, interest-serving placement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__state_centric_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_article_3_scope__state_centric_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(comm_tr_t0, observed).
narrative_ontology:measurement(comm_tr_t13, common_article_3_scope__state_centric_reading, theater_ratio, 13, 0.26).
narrative_ontology:measurement_basis(comm_tr_t13, observed).
narrative_ontology:measurement(comm_tr_t26, common_article_3_scope__state_centric_reading, theater_ratio, 26, 0.32).
narrative_ontology:measurement_basis(comm_tr_t26, observed).
narrative_ontology:measurement(comm_tr_t39, common_article_3_scope__state_centric_reading, theater_ratio, 39, 0.34).
narrative_ontology:measurement_basis(comm_tr_t39, observed).
narrative_ontology:measurement(comm_tr_t46, common_article_3_scope__state_centric_reading, theater_ratio, 46, 0.3).
narrative_ontology:measurement_basis(comm_tr_t46, observed).
narrative_ontology:measurement(comm_tr_t52, common_article_3_scope__state_centric_reading, theater_ratio, 52, 0.45).
narrative_ontology:measurement_basis(comm_tr_t52, observed).
narrative_ontology:measurement(comm_tr_t65, common_article_3_scope__state_centric_reading, theater_ratio, 65, 0.43).
narrative_ontology:measurement_basis(comm_tr_t65, observed).
narrative_ontology:measurement(comm_tr_t76, common_article_3_scope__state_centric_reading, theater_ratio, 76, 0.45).
narrative_ontology:measurement_basis(comm_tr_t76, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_article_3_scope__state_centric_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(comm_be_t0, observed).
narrative_ontology:measurement(comm_be_t13, common_article_3_scope__state_centric_reading, base_extractiveness, 13, 0.33).
narrative_ontology:measurement_basis(comm_be_t13, observed).
narrative_ontology:measurement(comm_be_t26, common_article_3_scope__state_centric_reading, base_extractiveness, 26, 0.36).
narrative_ontology:measurement_basis(comm_be_t26, observed).
narrative_ontology:measurement(comm_be_t39, common_article_3_scope__state_centric_reading, base_extractiveness, 39, 0.4).
narrative_ontology:measurement_basis(comm_be_t39, observed).
narrative_ontology:measurement(comm_be_t46, common_article_3_scope__state_centric_reading, base_extractiveness, 46, 0.42).
narrative_ontology:measurement_basis(comm_be_t46, observed).
narrative_ontology:measurement(comm_be_t52, common_article_3_scope__state_centric_reading, base_extractiveness, 52, 0.46).
narrative_ontology:measurement_basis(comm_be_t52, observed).
narrative_ontology:measurement(comm_be_t65, common_article_3_scope__state_centric_reading, base_extractiveness, 65, 0.45).
narrative_ontology:measurement_basis(comm_be_t65, observed).
narrative_ontology:measurement(comm_be_t76, common_article_3_scope__state_centric_reading, base_extractiveness, 76, 0.46).
narrative_ontology:measurement_basis(comm_be_t76, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_article_3_scope__state_centric_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(comm_su_t0, observed).
narrative_ontology:measurement(comm_su_t13, common_article_3_scope__state_centric_reading, suppression_requirement, 13, 0.38).
narrative_ontology:measurement_basis(comm_su_t13, observed).
narrative_ontology:measurement(comm_su_t26, common_article_3_scope__state_centric_reading, suppression_requirement, 26, 0.48).
narrative_ontology:measurement_basis(comm_su_t26, observed).
narrative_ontology:measurement(comm_su_t39, common_article_3_scope__state_centric_reading, suppression_requirement, 39, 0.5).
narrative_ontology:measurement_basis(comm_su_t39, observed).
narrative_ontology:measurement(comm_su_t46, common_article_3_scope__state_centric_reading, suppression_requirement, 46, 0.54).
narrative_ontology:measurement_basis(comm_su_t46, observed).
narrative_ontology:measurement(comm_su_t52, common_article_3_scope__state_centric_reading, suppression_requirement, 52, 0.62).
narrative_ontology:measurement_basis(comm_su_t52, observed).
narrative_ontology:measurement(comm_su_t65, common_article_3_scope__state_centric_reading, suppression_requirement, 65, 0.68).
narrative_ontology:measurement_basis(comm_su_t65, observed).
narrative_ontology:measurement(comm_su_t76, common_article_3_scope__state_centric_reading, suppression_requirement, 76, 0.7).
narrative_ontology:measurement_basis(comm_su_t76, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, icrc_customary_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'the scope of Common Article 3' decomposes into three structurally distinct readings of one kernel (common_article_3_scope): the state-centric treaty-threshold reading (this story), the expansive human-rights floor reading, and the ICRC customary-evolution reading. Each reading instantiates a different constraint with its own ε, beneficiary/victim structure, and victim set over the same fixed referent — the threshold-gated arrangement. The readings differ in ε because ε is a property of the reading, not the topic: the state-centric seat authors moderate extraction (the reading justifies the gap), the expansive seat authors high extraction (the gap is abandonment). This story links to both siblings because the state-centric reading is upstream in enforcement terms — state control of classification sets the operating conditions under which the sibling readings can gain traction — and the sibling stories carry their own ε and link back into the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
