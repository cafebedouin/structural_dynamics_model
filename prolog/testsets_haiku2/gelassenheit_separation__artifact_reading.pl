% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__artifact_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__artifact_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: gelassenheit_separation__artifact_reading
 *   human_readable: Gelassenheit Separation (Artifact Reading): Visible Distinction from English Society
 *   domain: religious/technological/social
 *
 * SUMMARY:
 *   The artifact reading of Gelassenheit separation is one interpretation of
 *   how a community maintains boundary distinctiveness from English secular
 *   society. Under this reading, technologies are evaluated primarily for
 *   their form (resemblance to worldly artifacts) rather than their function
 *   or relational impact. A solar panel is forbidden because it looks like an
 *   English/worldly technology, even if functionally isolated. Modern fabrics
 *   are rejected if they resemble English fashion, regardless of off-grid
 *   sustainability. The reading prioritizes visual markers as the boundary
 *   mechanism. This constraint is CLAIMED as tangled rope (genuine
 *   coordination function — boundary maintenance — plus asymmetric extraction
 *   from technology adopters and youth with functional needs). The authored
 *   metrics show high extraction (0.82) and extremely high suppression (0.91)
 *   because the constraint's persistence depends on actively enforcing
 *   form-based rejection despite functional arguments, and because
 *   alternatives (principle reading, consequence reading) are structurally
 *   present but excluded from consensus. Theater is substantial (0.67)
 *   because much enforcement activity performs boundary distinctiveness
 *   rather than solving the founding coordination problem.
 *
 * KEY AGENTS:
 *   - Community boundary maintainers (organized, identity-locked, agenda-setter): maintain the artifact-form boundary and adjudicate what resembles worldliness.
 *   - Technology adopters (moderate power, identity-locked, payer): bear the cost of sub-optimal technologies and social friction from enforcement.
 *   - Youth with functional needs (powerless, identity-locked, payer): carry suppression and functional harm; lack voice in interpretation.
 *   - Community enterprises (organized, constrained exit, beneficiary): gain competitive advantage from visible distinctiveness and bounded supply networks.
 *   - Principle reading holders (organized, excluded): present but silenced; would reframe technology evaluation around functional isolation rather than artifact form.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, 0.82).
domain_priors:suppression_score(gelassenheit_separation__artifact_reading, 0.91).
domain_priors:theater_ratio(gelassenheit_separation__artifact_reading, 0.67).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, theater_ratio, 0.67).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__artifact_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__artifact_reading, "Gelassenheit Separation (Artifact Reading): Visible Distinction from English Society").
narrative_ontology:topic_domain(gelassenheit_separation__artifact_reading, "religious/technological/social").

domain_priors:requires_active_enforcement(gelassenheit_separation__artifact_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__artifact_reading, 'a964ad2d-ce83-4e3f-898c-c0c524c0e887').
narrative_ontology:cs_kernel_codification('a964ad2d-ce83-4e3f-898c-c0c524c0e887', distributed).
narrative_ontology:cs_authority_grounding('a964ad2d-ce83-4e3f-898c-c0c524c0e887', lineage).
narrative_ontology:cs_interpretation_layer_present('a964ad2d-ce83-4e3f-898c-c0c524c0e887').
narrative_ontology:cs_reading_relation('a964ad2d-ce83-4e3f-898c-c0c524c0e887', gelassenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_reading_relation('a964ad2d-ce83-4e3f-898c-c0c524c0e887', gelassenheit_separation__consequence_reading, influences).
narrative_ontology:cs_axiom('a964ad2d-ce83-4e3f-898c-c0c524c0e887', foundational, separation_requires_visible_form_distinction).
narrative_ontology:cs_axiom_status(separation_requires_visible_form_distinction, holdable).
narrative_ontology:cs_axiom_grounding('a964ad2d-ce83-4e3f-898c-c0c524c0e887', separation_requires_visible_form_distinction, conventional).
narrative_ontology:cs_axiom('a964ad2d-ce83-4e3f-898c-c0c524c0e887', foundational, artifact_likeness_indicates_worldly_entanglement).
narrative_ontology:cs_axiom_status(artifact_likeness_indicates_worldly_entanglement, holdable).
narrative_ontology:cs_axiom_grounding('a964ad2d-ce83-4e3f-898c-c0c524c0e887', artifact_likeness_indicates_worldly_entanglement, deontological).
narrative_ontology:cs_reference_frame('a964ad2d-ce83-4e3f-898c-c0c524c0e887', mid_century_boundary_visibility_standard).
narrative_ontology:cs_drift_state('a964ad2d-ce83-4e3f-898c-c0c524c0e887', contemporary_technology_proliferation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a964ad2d-ce83-4e3f-898c-c0c524c0e887', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__artifact_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, community_boundary_maintainers).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, technology_adopters).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, youth_with_functional_needs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, community_cooperatives_and_enterprises).
narrative_ontology:constraint_vindicates(gelassenheit_separation__artifact_reading, separation_requires_visual_markers).
narrative_ontology:constraint_vindicates(gelassenheit_separation__artifact_reading, artifact_likeness_equals_worldliness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authority figures (bishops, deacons, elders) who interpret and enforce the artifact reading. They set the line on what 'resembles worldly artifacts' and adjudicate technological acceptability. They control social standing, church discipline, and the boundary between the community and English society. Their investment is in maintaining visible distinction — the community's identity marker is its refusal of 'worldly' form.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, community_boundary_maintainers, agenda_setter,
    organized, generational, identity_locked, local).

% Community members who want to use technologies that function better (modern fabrics, solar panels, efficient tools) but face rejection because their form resembles English/worldly artifacts, even when functionally isolated from worldly systems. They absorb the cost of sub-optimal technology and the social cost of boundary enforcement. Exit means shunning or permanent excommunication.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, technology_adopters, payer,
    moderate, biographical, identity_locked, local).

% Young people (especially those with disabilities or different abilities) for whom forbidden technologies provide real functional benefit or access. They carry both the enforcement suppression and the functional harm. Their options are full compliance, seeking private accommodation (identity boundary violation), or future exit. They lack voice in the interpretation process that declares what is 'worldly.'
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, youth_with_functional_needs, payer,
    powerless, biographical, identity_locked, local).

% Shared enterprises (bakeries, furniture shops, agricultural operations) that operate under the artifact reading. They maintain competitive advantage through community-internal trade (members buy preferentially from bounded suppliers) and collective reputation for distinctive practice. Modern forbidden technologies might improve efficiency but would visually signal capitulation to English standards, eroding the boundary value.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, community_cooperatives_and_enterprises, beneficiary,
    organized, generational, constrained, local).

% Outsiders (regulatory bodies, journalists, academics, marketers) who interpret the community's visible distinctiveness. Their recognition of the community as visibly 'other' is what makes separation socially meaningful at the boundary. They do not enforce internally but their observation anchors the constraint's external reference.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, english_society_observers, observer,
    powerful, biographical, analytical, national).

% Community members or leaders who hold the principle reading (technology acceptable if functionally isolated from worldly systems) but cannot voice this interpretation publicly without challenging the artifact reading's authority structure. They are present but silenced by the consensus requirement on community boundaries.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, principle_reading_holders, excluded,
    organized, generational, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__artifact_reading, community_boundary_maintainers).
narrative_ontology:fixing_cost_class(gelassenheit_separation__artifact_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a visible, legible boundary between the community and English secular society by enforcing consistent aesthetic markers (garment style, technology form, artifact appearance). This makes 'community membership' visibly recognizable and signals commitment to a shared boundary-maintenance practice across dispersed households and settlements.
% TRANSFER_FUNCTION: Moves the burden of technological suboptimality from the community-as-collective (which gains identity recognition and boundary distinctiveness) to individual technology adopters and those with functional needs (who forgo better tools, expend social capital on private accommodation, or accept reduced capability). The transfer is from adaptive capacity to boundary maintenance.
% ABSENT_VOICES: The principle reading adherents are structurally present but excluded from public consensus — they would argue that 'worldly entanglement' should track function (system participation), not form (artifact appearance). Youth with disabilities and those who experience functional harm from the rule are present but lack formal voice in boundary interpretation. Outside observers (scholars, regulators, journalists) are entirely excluded; their framing would emphasize coercion vs. voluntary distinctiveness.
% DISAPPEARANCE_RATIONALE: If the artifact reading enforcement vanished, the boundary would not disappear but would shift ground: some members would adopt technologies functionally (solar panels, modern textiles) while maintaining relational distinctiveness; enterprises would gain competitive efficiency; youth with functional needs would have options. The community would likely reorganize around the principle reading (functional isolation) or the consequence reading (relational practice), fundamentally altering how separation is enacted.
% FOUNDING_PROBLEM: In the mid-20th century, as consumer technologies proliferated and English suburban culture intensified around consumption practices, the community faced erosion of visible distinctiveness. Younger members adopted English fashions and modern conveniences; outside observers could not easily identify community boundaries. The constraint was erected to make separation visually legible — a response to the specific historical problem of cultural indistinguishability.
% FOUNDING_PROBLEM_CORROBORATION: Community historians and elders attest the founding problem was acute and the artifact reading solved it (visibility and cohesion recovered). Scholars of Anabaptist tradition note the founding problem was real but argue it was a 20th-century phenomenon, not inherent to Gelassenheit theology. Principle-reading holders (internal voices) attest the founding problem (indistinguishability) is solved by the principle reading with lower functional cost. No independent corroboration from outside the benefiting boundary-maintainers acknowledges the founding problem as requiring aesthetic suppression rather than functional or relational enforcement.
narrative_ontology:disappearance_verdict(gelassenheit_separation__artifact_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__artifact_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__artifact_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gelassenheit_separation__artifact_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__artifact_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__artifact_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gelassenheit_separation__artifact_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness trajectory (0.45 → 0.82) tracks the accumulation of rejected technologies as the technological surface of English society expanded post-1950. Early in the interval, the constraint was less extractive because fewer technologies collided with the boundary rule; by 2026, solar, wind, modern textiles, and medical devices all trigger artifact-form rejection. The suppression requirement rises (0.68 → 0.91) because enforcement intensity increased as the functional cost became harder to deny — more active discipline, more careful gatekeeping, more exclusion of alternative framings. Theater rises (0.32 → 0.67) because the ratio of enforcement-for-distinctiveness to enforcement-for-coordination deteriorated: early enforcement could claim to protect against genuine system entanglement, but by 2026 the rule forbids purely functional, off-grid technologies (solar panels on Amish-owned property with no grid connection), exposing the form-over-function priority. The measurements share one time grid: every metric is authored at every examined point across the 76-year interval. The cyclical pattern — tensions mount as youth adopt forbidden technologies, enforcement tightens, shunning occurs, then a few years of lowered friction until the next technological wave — is visible in the smoothed series but not explicitly marked as cyclical here; that pattern appears in the broader institutional history.
 *
 * PERSPECTIVAL GAP:
 *   From the boundary maintainers' seat, the artifact reading preserves genuine coordination (community visibility, relational cohesion, meaningful separation). From the technology adopters' seat, the same structure operates as enforced deprivation justified by form-based rules that ignore function. From the principle reading holders' seat, the boundary is overspecified — it prioritizes appearances rather than the actual structural isolation the founders meant to preserve. The engine should compute these as different constraint types from the same sitting. The boundary maintainers' seat (identity-locked, organized, agenda-setter) should compute as rope-flavored (they coordinate and benefit without extracting from each other). The payer seats (technology adopters, youth with needs) should compute as snare-flavored (high suppression, high extraction, constrained exit). The principle reading holders sit in a liminal position — they are present, organized, and identity-locked, but explicitly excluded from consensus, which suppresses their alternative framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The boundary maintainers are structural beneficiaries (d ≈ 0.1): they set the rules, control interpretation, and gain community identity recognition. Their exit is theoretically unconstrained but practically fused to their role. Technology adopters are structural targets (d ≈ 0.85): they bear the functional cost, face social discipline, and have no voice in the rule that constrains them. Identity lock is complete — leaving the community is the only way to access better technology. Youth with functional needs carry even higher directionality (d ≈ 0.95) because they lack voice and face both suppression and functional harm. Community enterprises are complex (d ≈ 0.3): they benefit from the boundary-maintained market but are also constrained by the rule — they would adopt more efficient technologies if permitted. The principle reading holders should compute with high d if their voice is excluded (trapped, identity-locked, organized but silenced) — their d would approach 0.75 despite being nominally insiders.
 *
 * MANDATROPHY ANALYSIS:
 *   The artifact reading shows strong mandatrophy signals. The founding problem (visibility during cultural indistinguishability, 1950–1970) is largely solved by the constraint's existence — outsiders consistently recognize the community's distinctiveness. Yet enforcement has not relaxed; instead, it has intensified (suppression rose from 0.68 to 0.91). Theater accumulation (0.32 → 0.67) indicates the rule now performs distinctiveness for its own sake rather than solving the coordination problem it was erected to address. The mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges) triggers the R5 zombie flag: the problem is no longer live (visibility is secure), yet the constraint persists and the lives that depend on it have reorganized around its continuation. The constraint has become a technology-suppression mechanism that coincidentally maintains boundaries rather than a boundary-maintenance mechanism that incidentally constrains technology. Mandatrophy is not resolved — the rule persists because the boundary-maintainers' power depends on its enforcement, not because the founding problem still requires solving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    form_vs_function_boundary,
    'Is the artifact reading''s form-based distinction (resemblance to worldly artifacts) coherent as a theological or practical principle, or is it a drift from the original functional-isolation principle?',
    'Genealogical analysis of the constraint''s emergence and evolution in community writings, sermons, and discipline records. Historical comparison to earlier statements of separation doctrine. Internal dispute: the principle reading holders possess competing interpretations of the founding doctrine.',
    'If form-based rejection is recognized as drift from function-based isolation, the constraint would be subject to reinterpretation pressure (principle reading would gain legitimacy). If form-based rejection is defended as essential to visibility, the constraint persists. If both are acknowledged as valid but in tension, the constraint becomes explicitly contestable rather than appearing consensual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(form_vs_function_boundary, conceptual, 'Whether the artifact reading represents the authentic original principle or a subsequent historical drift toward aesthetic enforcement.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the observed suppression of alternative readings (principle, consequence) a result of structural enforcement (threat of shunning, loss of social standing) or internalized acceptance of the artifact reading as correct?',
    'Post-exit follow-up: do members who leave the community and encounter the principle reading later recognize it as an alternative they had considered suppressing, or as a completely foreign idea? Do they report having felt pressure not to voice alternative interpretations while inside the community?',
    'If suppression is primarily structural, the constraint''s effective suppression could be reduced by removing enforcement pressure (though identity lock would remain). If suppression is primarily internalized, the constraint''s operation is more entrenched — members would likely remain opposed to alternative readings even if external enforcement were removed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether the silencing of principle and consequence readings is externally enforced or internally accepted.').

omega_variable(
    founding_problem_adequately_solved,
    'Is the founding problem (visibility and boundary distinctiveness during the mid-20th century cultural flux) still live, or does the constraint now persist despite the problem being substantially solved?',
    'Comparative ethnography with English secular communities and with Gelassenheit communities that have adopted the principle reading. Do the principle-reading communities experience erosion of boundary distinctiveness? Do principle-reading adopters remain visibly recognizable as community members?',
    'If the founding problem is solved (visibility is secure regardless of technology adoption), the constraint becomes a case of mandatrophy inertia — it persists because the authority structure benefits from its continuation, not because the coordination problem requires it. This is the reclassification trigger for piton or snare. If the founding problem is still live (boundary visibility depends on continued form-based exclusion), the constraint retains legitimate coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_adequately_solved, empirical, 'Whether the founding boundary-visibility problem persists as an open coordination challenge or is solved and the constraint now extracts absent any remaining problem.').

omega_variable(
    reading_coexistence_foreclosure_ambiguity,
    'Do the principle reading and the artifact reading logically foreclose each other within a single theological framework, or can both be held as different emphases within a single commitment to separation?',
    'Theological textual analysis and internal community debate. If principle reading holders are theologically forced to deny artifact reading''s core premise, foreclosure applies. If they can acknowledge artifact reading as a valid but overweighted emphasis, coexistence applies.',
    'If foreclosure: one reading must eventually displace the other; current coexistence is temporary political equilibrium. If coexistence: the readings can persist indefinitely as different communities'' emphasis, or as a productive internal tension. This determines whether the artifact reading''s suppression of the principle reading is structural exclusion (ongoing constraint cost) or organizational boundary-setting (acceptable differentiation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_foreclosure_ambiguity, conceptual, 'Whether the principle and artifact readings logically foreclose each other or can coexist within a single theological tradition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__artifact_reading, 1950, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t1950, gelassenheit_separation__artifact_reading, theater_ratio, 1950, 0.32).
narrative_ontology:measurement_basis(gela_tr_t1950, observed).
narrative_ontology:measurement(gela_tr_t1970, gelassenheit_separation__artifact_reading, theater_ratio, 1970, 0.44).
narrative_ontology:measurement_basis(gela_tr_t1970, observed).
narrative_ontology:measurement(gela_tr_t1990, gelassenheit_separation__artifact_reading, theater_ratio, 1990, 0.54).
narrative_ontology:measurement_basis(gela_tr_t1990, observed).
narrative_ontology:measurement(gela_tr_t2008, gelassenheit_separation__artifact_reading, theater_ratio, 2008, 0.61).
narrative_ontology:measurement_basis(gela_tr_t2008, observed).
narrative_ontology:measurement(gela_tr_t2018, gelassenheit_separation__artifact_reading, theater_ratio, 2018, 0.65).
narrative_ontology:measurement_basis(gela_tr_t2018, observed).
narrative_ontology:measurement(gela_tr_t2026, gelassenheit_separation__artifact_reading, theater_ratio, 2026, 0.67).
narrative_ontology:measurement_basis(gela_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(gela_be_t1950, gelassenheit_separation__artifact_reading, base_extractiveness, 1950, 0.45).
narrative_ontology:measurement_basis(gela_be_t1950, observed).
narrative_ontology:measurement(gela_be_t1970, gelassenheit_separation__artifact_reading, base_extractiveness, 1970, 0.58).
narrative_ontology:measurement_basis(gela_be_t1970, observed).
narrative_ontology:measurement(gela_be_t1990, gelassenheit_separation__artifact_reading, base_extractiveness, 1990, 0.71).
narrative_ontology:measurement_basis(gela_be_t1990, observed).
narrative_ontology:measurement(gela_be_t2008, gelassenheit_separation__artifact_reading, base_extractiveness, 2008, 0.78).
narrative_ontology:measurement_basis(gela_be_t2008, observed).
narrative_ontology:measurement(gela_be_t2018, gelassenheit_separation__artifact_reading, base_extractiveness, 2018, 0.8).
narrative_ontology:measurement_basis(gela_be_t2018, observed).
narrative_ontology:measurement(gela_be_t2026, gelassenheit_separation__artifact_reading, base_extractiveness, 2026, 0.82).
narrative_ontology:measurement_basis(gela_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t1950, gelassenheit_separation__artifact_reading, suppression_requirement, 1950, 0.68).
narrative_ontology:measurement_basis(gela_su_t1950, observed).
narrative_ontology:measurement(gela_su_t1970, gelassenheit_separation__artifact_reading, suppression_requirement, 1970, 0.76).
narrative_ontology:measurement_basis(gela_su_t1970, observed).
narrative_ontology:measurement(gela_su_t1990, gelassenheit_separation__artifact_reading, suppression_requirement, 1990, 0.84).
narrative_ontology:measurement_basis(gela_su_t1990, observed).
narrative_ontology:measurement(gela_su_t2008, gelassenheit_separation__artifact_reading, suppression_requirement, 2008, 0.88).
narrative_ontology:measurement_basis(gela_su_t2008, observed).
narrative_ontology:measurement(gela_su_t2018, gelassenheit_separation__artifact_reading, suppression_requirement, 2018, 0.9).
narrative_ontology:measurement_basis(gela_su_t2018, observed).
narrative_ontology:measurement(gela_su_t2026, gelassenheit_separation__artifact_reading, suppression_requirement, 2026, 0.91).
narrative_ontology:measurement_basis(gela_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__artifact_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__artifact_reading, 0.14).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__principle_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__consequence_reading).

% DUAL FORMULATION NOTE:
% The gelassenheit_separation kernel instantiates three distinct constraints, each implementing a different reading of 'separation' from English society. The artifact reading prioritizes visible form (technologies forbidden if resembling worldly artifacts); the principle reading prioritizes functional isolation (technologies acceptable if structurally decoupled); the consequence reading prioritizes relational impact (technologies evaluated by effect on mutual aid, visiting, rootedness). Each reading has different beneficiaries, victims, and extractiveness profiles. They are linked by network.affects_constraints as a constraint family because the artifact reading's authority structure explicitly excludes and suppresses the principle and consequence readings, creating structural coupling. The three readings compete for institutional legitimacy within the community.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gelassenheit_separation__artifact_reading, organized, 0.78).
constraint_indexing:directionality_override(gelassenheit_separation__artifact_reading, powerless, 0.94).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
