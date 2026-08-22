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
 *   human_readable: Gelassenheit Separation via Artifact Appearance (Artifact Reading)
 *   domain: religious/cultural/technological
 *
 * SUMMARY:
 *   The artifact-reading of Gelassenheit separation is one of three competing
 *   interpretations within North American Anabaptist communities about what
 *   'separation from worldly society' demands. This reading holds that
 *   separation is primarily a matter of visible distinction — the material
 *   culture must not resemble English/non-Anabaptist equivalents, regardless
 *   of function. Under this reading, a solar panel is forbidden not because
 *   solar power is inherently worldly, but because solar panels look like
 *   modern industrial artifacts; synthetic fabrics are forbidden not because
 *   synthetic material is immoral, but because Dacron and polyester are
 *   visually associated with worldly fashion. The constraint achieves high
 *   extractiveness (0.82) because it suppresses practical technologies that
 *   improve living conditions, and it achieves high suppression (0.91)
 *   because enforcement relies on the threat of Bann (total community
 *   expulsion). The temporal measurements show steady extraction accumulation
 *   over the 1920–2025 interval: as surrounding technology advanced
 *   (electricity, automobiles, industrial agriculture), the constraint's
 *   suppressive force hardened to maintain the visual boundary. Theater has
 *   also risen (0.15 to 0.44) as enforcement activity increasingly defends
 *   the aesthetic boundary rather than addressing substantive theological
 *   questions.
 *
 * KEY AGENTS:
 *   - community_leadership: Bishops and church councils maintaining the artifact-reading interpretation and enforcing the prohibition on technologies that visually resemble worldly artifacts
 *   - individuals_pursuing_practical_efficiency: Farmers and household heads who recognize the practical gains from solar power and synthetic materials but face expulsion if they adopt them
 *   - off_grid_households: The most constrained group, combining practical need for efficiency with maximum visibility (permanent installations cannot be hidden) and maximum suppression (identity-locked exit)
 *   - younger_generation: Born into the tradition, witnessing the irrationality of the rule as neighboring Mennonite and Amish communities adopt the same technologies, experiencing the constraint as oppressive
 *   - consequence_reading_practitioners: Alternative Anabaptist communities that permit technologies based on their effect on community practices rather than artifact appearance
 *   - principle_reading_practitioners: Alternative communities that permit functionally isolated technologies (like stand-alone solar arrays) as acceptable separation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, 0.82).
domain_priors:suppression_score(gelassenheit_separation__artifact_reading, 0.91).
domain_priors:theater_ratio(gelassenheit_separation__artifact_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__artifact_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__artifact_reading, "Gelassenheit Separation via Artifact Appearance (Artifact Reading)").
narrative_ontology:topic_domain(gelassenheit_separation__artifact_reading, "religious/cultural/technological").

domain_priors:requires_active_enforcement(gelassenheit_separation__artifact_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__artifact_reading, 'e491ef12-a3c6-4cb4-8669-233e95d30aea').
narrative_ontology:cs_kernel_codification('e491ef12-a3c6-4cb4-8669-233e95d30aea', distributed).
narrative_ontology:cs_authority_grounding('e491ef12-a3c6-4cb4-8669-233e95d30aea', lineage).
narrative_ontology:cs_interpretation_layer_present('e491ef12-a3c6-4cb4-8669-233e95d30aea').
narrative_ontology:cs_reading_relation('e491ef12-a3c6-4cb4-8669-233e95d30aea', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_reading_relation('e491ef12-a3c6-4cb4-8669-233e95d30aea', gelassenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_axiom('e491ef12-a3c6-4cb4-8669-233e95d30aea', foundational, visual_distinctiveness_is_separation).
narrative_ontology:cs_axiom_status(visual_distinctiveness_is_separation, holdable).
narrative_ontology:cs_axiom_grounding('e491ef12-a3c6-4cb4-8669-233e95d30aea', visual_distinctiveness_is_separation, conventional).
narrative_ontology:cs_axiom('e491ef12-a3c6-4cb4-8669-233e95d30aea', foundational, artifact_resemblance_requires_prohibition).
narrative_ontology:cs_axiom_status(artifact_resemblance_requires_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('e491ef12-a3c6-4cb4-8669-233e95d30aea', artifact_resemblance_requires_prohibition, empirically_contingent).
narrative_ontology:cs_reference_frame('e491ef12-a3c6-4cb4-8669-233e95d30aea', visible_anabaptist_identity_through_artifact_distinctiveness).
narrative_ontology:cs_drift_state('e491ef12-a3c6-4cb4-8669-233e95d30aea', contemporary_industrial_modernity, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e491ef12-a3c6-4cb4-8669-233e95d30aea', '2026-06-12T14:30:00Z').
narrative_ontology:cs_kernel_id(gelassenheit_separation__artifact_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, community_leadership).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, visible_separation_doctrine).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, individuals_pursuing_practical_efficiency).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, off_grid_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, younger_generation_modernizers).
narrative_ontology:constraint_vindicates(gelassenheit_separation__artifact_reading, visual_distinctiveness_as_separation_marker).
narrative_ontology:constraint_vindicates(gelassenheit_separation__artifact_reading, artifact_resemblance_prohibition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Church leaders and community bishops enforce the artifact-appearance rule: they review technology proposals, reject those that visually resemble worldly artifacts (solar panels, synthetic fabrics, electric motors), and maintain the visible distinctiveness boundary. They justify the rule as preserving separation from English society and defending Gelassenheit (yielding to divine will through non-resistance to tradition). They collect cultural authority and community cohesion from the rule's enforcement.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, community_leadership, agenda_setter,
    organized, generational, mobile, regional).

% Household heads and farmers who recognize practical efficiency gains from solar panels, synthetic work clothing, or efficient electric pumps. They bear the cost of the prohibition: continued manual labor, higher fuel costs, heating inefficiency, and reduced productivity. Exit means formal shunning (Bann) — expulsion from the community and severance of all social ties with family and neighbors, making exit prohibitively costly for most.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, individuals_pursuing_practical_efficiency, payer,
    moderate, biographical, constrained, local).

% Families committed to off-grid self-sufficiency who would benefit most from renewable energy and modern materials but face the strictest enforcement because they cannot hide the technology use. Off-grid status makes the visual prohibition especially binding: solar panels and efficient fabrics are not hidden infrastructure but permanent, visible installations. Their identity is constituted through community membership and religious tradition; exit is psychologically unthinkable even when technically possible.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, off_grid_households, payer,
    powerless, biographical, identity_locked, local).

% Young people born into the community who witness the efficiency gains from forbidden technologies in adjacent communities (hybrid Mennonite groups, secular rural populations) and experience the constraint as irrational. They face enforcement pressure from parents and bishops. Some leave; many stay but nurse resentment. Their voice is largely excluded from rule-setting because decision-making authority rests with married men of long standing.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, younger_generation_modernizers, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__artifact_reading, younger_generation_modernizers, observer).

% Bishops and communities that interpret separation via the consequence-reading (technology acceptable if it preserves mutual aid and geographic rootedness) have allowed solar power and synthetic fabrics in off-grid settings. They would argue for technology evaluation on practical and social grounds rather than artifact appearance. They are excluded from this artifact-reading community's decision-making; where they coexist geographically, they model an alternative practice the artifact-reading community actively rejects.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, consequence_reading_practitioners, excluded,
    organized, generational, constrained, regional).

% Communities that interpret separation via the principle-reading (technology acceptable if functionally isolated from worldly systems) accept some modern artifacts if they are stand-alone installations. They would permit solar arrays and efficient fabrics because they represent functional isolation, not entanglement. They are excluded from artifact-reading rule-setting and model a competing constraint interpretation.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, principle_reading_practitioners, excluded,
    organized, generational, constrained, regional).

% Not a real actor: the abstract fact that efficient technologies improve work output and reduce human suffering. Carried as a beneficiary because the constraint's opponents vindicate this proposition, not the leadership. It collects nothing but helps frame what the rule suppresses.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, biomechanical_efficiency_function, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(gelassenheit_separation__artifact_reading, biomechanical_efficiency_function).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__artifact_reading, community_leadership).
narrative_ontology:fixing_cost_class(gelassenheit_separation__artifact_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains visible boundary markers that distinguish the community from surrounding English society: restricts technology adoption to methods that do not create visual resemblance to worldly artifacts, preserving a recognizable Anabaptist/Mennonite material culture that signals membership and identity.
% TRANSFER_FUNCTION: Transfers the efficiency gains from solar power, synthetic fabrics, and modern equipment to the community as a whole (via the authority of a unified tradition) and redistributes burden of manual labor unequally — those with less political power bear the highest cost of the prohibition.
% ABSENT_VOICES: Younger generation, off-grid households, and alternative-reading communities are structurally excluded from deciding what counts as 'resemblance.' Industrial-society engineers and efficiency advocates outside the community would object on grounds of rationality and harm reduction but are not in the conversation by design.
% DISAPPEARANCE_RATIONALE: If the artifact-appearance prohibition vanished, solar installations would appear within months in off-grid communities; synthetic work clothing would displace cotton and wool; labor productivity would rise sharply. The constraint's disappearance would not eliminate Gelassenheit theology but would decouple visible distinctiveness from separation doctrine, forcing a theological reconstruction. The community would reorganize around the consequence- or principle-readings instead.
% FOUNDING_PROBLEM: In the 16th century, Anabaptist separation from Catholic/Protestant Christendom required visible markers: plain dress, simple furnishings, rejection of oaths and worldly status symbols. In the 20th and 21st centuries, the artifact-reading interpretation extended this historical principle to technological form: 'worldly' artifacts — those that resemble English/non-Anabaptist versions — become forbidden not by function but by appearance.
% FOUNDING_PROBLEM_CORROBORATION: The artifact-reading leadership attests the founding problem is live: technology visibility marks separation and must be defended. Historians of Anabaptism outside the community note that the founding problem (16th-century religious persecution and need for visible boundary) was structurally resolved by the 19th century in most North American communities; the constraint persists as reification of historical identity rather than active response to current threats. Consequence- and principle-reading communities attest the problem has been superseded by more nuanced theological frameworks. Younger community members, in interviews outside formal structures, express skepticism about the founding problem's continued relevance.
narrative_ontology:disappearance_verdict(gelassenheit_separation__artifact_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__artifact_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__artifact_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The artifact-reading achieves structural high extractiveness through three mechanisms: (1) it suppresses technologies that are functionally beneficial but aesthetically modern, creating genuine opportunity cost; (2) it redistributes the burden of this suppression unequally — off-grid households bear the highest cost because they cannot hide installations and face the strongest enforcement pressure; (3) it transfers the cultural authority that comes from 'preserving distinctiveness' to the community leadership that administers the rule. Suppression is extremely high (0.91) because enforcement is backed by Bann — total expulsion from family and community — making exit prohibitively costly for most. Theater has risen over the interval because the founding problem (defensive separation from persecution) has become irrelevant; enforcement now primarily defends the aesthetic marker itself rather than addressing substantive religious doctrine. The measurement trajectory shows extraction accumulating as technology options expanded: in 1920, the constraint suppressed relatively crude technologies; by 2025, it suppresses high-efficiency solar, synthetic insulation, and modern farming equipment, making the opportunity cost of compliance much higher. The artifact-reading is vulnerable to the natural-law vs. constructed omega because the rule's specificity to industrial-age artifacts suggests historical elaboration rather than transmission — the 16th-century Anabaptist sources that justify the general principle of separation do not prescribe prohibition on solar panels.
 *
 * PERSPECTIVAL GAP:
 *   The leadership and the payer seats should compute dramatically differently. From the leadership position, the constraint preserves Gelassenheit and community distinctiveness — a genuine coordination function protecting cultural identity. From the off-grid household position, the constraint is pure extraction: it forbids the technologies that would most improve their lives, backed by the threat of Bann. The younger generation sits in a complicated middle position: they see the constraint's irrationality (alternative communities prosper with the same technologies) but are identity-locked into compliance. The engine should compute the artifact-reading as tangled_rope from the leadership seat (coordination + active enforcement) and as snare or high-extraction piton from the payer seats (enforced suppression without genuine coordination benefit). This per-seat divergence is the structural evidence that the claim/metric gap is real.
 *
 * DIRECTIONALITY LOGIC:
 *   Community leadership benefits from the constraint (collects cultural authority, maintains governance role) — d near 0.0 (beneficiary). Individuals pursuing efficiency and off-grid households bear costs (lose technology options, face suppression for violation) — d near 1.0 (target). The younger generation and consequence/principle-reading practitioners are excluded from rule-making despite living the constraint's consequences — d somewhere between 0.5 and 1.0 depending on how much they internalize the rule. The constraint is enforced actively (Bann threat) and the exit options are severely constrained for most (identity_locked for off-grid households, constrained for efficiency-pursuers). No directionality overrides are needed; the structural derivation captures the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was defensive separation from persecution and religious outsiders — a live coordination problem in the 16th and early 20th centuries. By the 21st century, the problem has been substantially solved: North American Anabaptist communities are legally protected, socially tolerated, and no longer subject to persecution. The constraint persists not because the problem is live but because enforcement machinery became institutionalized and aesthetic distinctiveness became an end in itself. This is a classic mandatrophy case: the mandate (preserve separation from persecution through visible distinctiveness) has outlived its function, but enforcement persists as theater and cultural control. The measurement trajectory shows this: suppression_requirement and theater_ratio both rise over the interval as the founding problem becomes more historical and less urgent. The constraint could be classified as tangled_rope (from leadership) or piton (from payer seats) depending on how much the coordination benefit of cultural distinctiveness is valued. The (founding_problem_status=dead x disappearance_verdict=world_rearranges) mismatch flags a capture/zombie scenario: the founding problem is gone, but the arrangement persists and would reorganize if removed, which suggests the constraint is maintained for leadership benefit, not community necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_distinction,
    'Is the artifact-appearance rule a genuine commitment to Gelassenheit (yielding), or a reification of 20th-century cultural aesthetics now treated as eternal theological principle?',
    'Genealogical analysis: compare the rule as stated in 1960s enforcement documents with Anabaptist primary sources from the 16th–18th centuries. If the rule''s specificity to industrial-age artifacts (solar panels, synthetic fabrics) is documented as an elaboration rather than a transmission, the rule is constructed, not natural.',
    'If constructed, the rule is a false summit — a human choice benefiting leadership, disguised as divine principle. If natural (genuinely transmitted), it is a legitimate cultural boundary. The engine''s classification and the FSM signature would detect the false summit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_distinction, empirical, 'Whether the artifact-appearance rule is inherited theological principle or elaborated cultural reification.').

omega_variable(
    reading_kernel_identity_ambiguity,
    'What is the kernel that the artifact-reading instantiates? Is it ''separation as visible distinctiveness'' (unique to this reading), or a more general principle about separation that the artifact-reading interprets maximally?',
    'Compare foundational texts cited by all three readings. If all three cite the same passages and diverge only in interpretation, the kernel is general (separation) and three readings are three interpretations. If the artifact-reading cites distinct historical episodes or doctrinal sources, the kernel may be artifact-specific.',
    'If the kernel is general, the three readings coexist as live alternatives; if the kernel is artifact-specific, the artifact-reading forecloses the others. The cs_structure.reading_relations assignment hinges on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_identity_ambiguity, conceptual, 'Whether the contested kernel is general separation or artifact-specific prohibition.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of technology adoption structurally enforced (legal shunning, loss of participation rights) or internalized (community members enforce the rule on themselves through shame, identity fusion with the tradition)?',
    'Post-exit trajectory study: track individuals who leave the community over technology disputes and measure whether suppression drops, stays constant, or inverts. If suppression persists after exit (individuals still avoid solar panels, feel shame about synthetic clothing), suppression is internalized. If suppression disappears after exit, it is structural.',
    'If suppression is primarily internalized, the measured suppression score (0.91) understates the constraint''s binding force — individuals carry the suppression with them after exit. If structural, the score reflects enforcement machinery accurately. Internalized suppression would make the constraint more extractive than metrics show.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether the constraint''s suppression is structurally enforced (shunning, exclusion) or internalized (identity fusion, shame).').

omega_variable(
    reading_divergence_mechanism,
    'The three readings coexist within overlapping communities. What preserves their distinctness and prevents convergence toward a single dominant reading?',
    'Institutional mapping: identify which geographic regions, bishops, and communities hold each reading. If readings cluster (artifact-reading in communities with high English contact, principle-reading in isolated areas), institutional separation explains the divergence. If readings are mixed (same bishop holds both readings), consensus mechanisms are preventing drift.',
    'High institutional separation makes readings structurally robust and unlikely to collapse into one; mixed institutional context suggests readings are in active renegotiation and one may win out. The cs_structure.reading_relations assignment (coexists_with vs. influences) depends on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_divergence_mechanism, empirical, 'What institutional mechanisms preserve reading distinctness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__artifact_reading, 1920, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t1920, gelassenheit_separation__artifact_reading, theater_ratio, 1920, 0.15).
narrative_ontology:measurement_basis(gela_tr_t1920, projected).
narrative_ontology:measurement(gela_tr_t1960, gelassenheit_separation__artifact_reading, theater_ratio, 1960, 0.24).
narrative_ontology:measurement_basis(gela_tr_t1960, observed).
narrative_ontology:measurement(gela_tr_t1985, gelassenheit_separation__artifact_reading, theater_ratio, 1985, 0.32).
narrative_ontology:measurement_basis(gela_tr_t1985, observed).
narrative_ontology:measurement(gela_tr_t2005, gelassenheit_separation__artifact_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement_basis(gela_tr_t2005, observed).
narrative_ontology:measurement(gela_tr_t2020, gelassenheit_separation__artifact_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement_basis(gela_tr_t2020, observed).
narrative_ontology:measurement(gela_tr_t2025, gelassenheit_separation__artifact_reading, theater_ratio, 2025, 0.44).
narrative_ontology:measurement_basis(gela_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(gela_be_t1920, gelassenheit_separation__artifact_reading, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement_basis(gela_be_t1920, projected).
narrative_ontology:measurement(gela_be_t1960, gelassenheit_separation__artifact_reading, base_extractiveness, 1960, 0.68).
narrative_ontology:measurement_basis(gela_be_t1960, observed).
narrative_ontology:measurement(gela_be_t1985, gelassenheit_separation__artifact_reading, base_extractiveness, 1985, 0.76).
narrative_ontology:measurement_basis(gela_be_t1985, observed).
narrative_ontology:measurement(gela_be_t2005, gelassenheit_separation__artifact_reading, base_extractiveness, 2005, 0.79).
narrative_ontology:measurement_basis(gela_be_t2005, observed).
narrative_ontology:measurement(gela_be_t2020, gelassenheit_separation__artifact_reading, base_extractiveness, 2020, 0.81).
narrative_ontology:measurement_basis(gela_be_t2020, observed).
narrative_ontology:measurement(gela_be_t2025, gelassenheit_separation__artifact_reading, base_extractiveness, 2025, 0.82).
narrative_ontology:measurement_basis(gela_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t1920, gelassenheit_separation__artifact_reading, suppression_requirement, 1920, 0.68).
narrative_ontology:measurement_basis(gela_su_t1920, projected).
narrative_ontology:measurement(gela_su_t1960, gelassenheit_separation__artifact_reading, suppression_requirement, 1960, 0.78).
narrative_ontology:measurement_basis(gela_su_t1960, observed).
narrative_ontology:measurement(gela_su_t1985, gelassenheit_separation__artifact_reading, suppression_requirement, 1985, 0.84).
narrative_ontology:measurement_basis(gela_su_t1985, observed).
narrative_ontology:measurement(gela_su_t2005, gelassenheit_separation__artifact_reading, suppression_requirement, 2005, 0.88).
narrative_ontology:measurement_basis(gela_su_t2005, observed).
narrative_ontology:measurement(gela_su_t2020, gelassenheit_separation__artifact_reading, suppression_requirement, 2020, 0.9).
narrative_ontology:measurement_basis(gela_su_t2020, observed).
narrative_ontology:measurement(gela_su_t2025, gelassenheit_separation__artifact_reading, suppression_requirement, 2025, 0.91).
narrative_ontology:measurement_basis(gela_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__artifact_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__artifact_reading, 0.12).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__consequence_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__principle_reading).

% DUAL FORMULATION NOTE:
% The gelassenheit_separation kernel is instantiated by three structurally distinct constraint stories, one per reading. The artifact-reading prioritizes visible markers and forbids technologies by appearance; the consequence-reading permits technologies that preserve community practices; the principle-reading permits functionally isolated technologies. All three share the same founding problem (16th-century Anabaptist separation) but produce different ε values and victim sets. The three constraints are linked by network.affects_constraints because each reading's adoption or decline creates structural pressure on the others — adoption of consequence-reading in one community influences neighboring artifact-reading communities to justify their stricter rule or relax it. No reading logically forecloses another within a single party's framework, but they compete for institutional adoption across the community landscape.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
