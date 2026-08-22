% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__expansive_universalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__expansive_universalist, []).

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
 *   constraint_id: equality_clause_scope__expansive_universalist
 *   human_readable: Expansive Universalist Reading of Equality Clause
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The expansive universalist reading of the equality clause asserts that
 *   equality is a self-evident universal truth applying to all humans
 *   regardless of historical exclusions. This is one of three contested
 *   readings of the same constitutional kernel (the equality principle
 *   embedded in the Declaration of Independence, the 14th Amendment, and
 *   various state constitutions). This reading claims that any historical
 *   limitation to a narrow class (propertied males in the 18th century)
 *   represents hypocrisy or error, not legitimate constitutional boundaries.
 *   The reading has become institutionalized through judicial decisions,
 *   civil rights movements, and constitutional amendments; it now constitutes
 *   the dominant interpretive frame in contemporary U.S. jurisprudence.
 *   However, it remains contested by originalist readings (which treat the
 *   18th-century ratification as binding) and progressive textualist readings
 *   (which require democratic amendment for scope expansion rather than
 *   judicial interpretation). The constraint's extractiveness is moderate-low
 *   (0.38) because the reading coordinates real harm (historical exclusion)
 *   with a genuine normative claim (universal human equality), but requires
 *   active judicial enforcement to override contrary legal precedents and
 *   political resistance from traditional power holders. Suppression is
 *   moderate (0.52) because the reading faces real resistance from
 *   originalist jurisprudence and constitutional traditionalists, and must be
 *   sustained through continued judicial activism and social pressure.
 *
 * KEY AGENTS:
 *   - Historically excluded groups (women, racial minorities, sexual minorities, economically dispossessed): bearers of historical exclusion; presumptive beneficiaries of universal-equality interpretation.
 *   - Civil rights advocates and progressive legal institutions: beneficiaries who gain legitimacy and agenda-setting power from the expansive reading.
 *   - Traditional power holders (inheritors of 18th-century constitutional settlement): payers who lose inherited status and legal privileges as equality expands.
 *   - Originalist judges and scholars: payers who face delegitimization of their interpretive methodology.
 *   - Constitutional judges: agenda-setters with identity-locked institutional authority to declare what the Constitution means.
 *   - Democratic amendment proponents: excluded voices who argue for legitimacy through democratic process rather than judicial interpretation.
 *   - Analytical observers: scholars examining coherence and strategic consequences of the reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, 0.38).
domain_priors:suppression_score(equality_clause_scope__expansive_universalist, 0.52).
domain_priors:theater_ratio(equality_clause_scope__expansive_universalist, 0.21).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, extractiveness, 0.38).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, theater_ratio, 0.21).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__expansive_universalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__expansive_universalist, "Expansive Universalist Reading of Equality Clause").
narrative_ontology:topic_domain(equality_clause_scope__expansive_universalist, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(equality_clause_scope__expansive_universalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__expansive_universalist, '095b7ea1-4284-40b4-92c2-ddbd9f1e9cda').
narrative_ontology:cs_kernel_codification('095b7ea1-4284-40b4-92c2-ddbd9f1e9cda', fixed_text).
narrative_ontology:cs_authority_grounding('095b7ea1-4284-40b4-92c2-ddbd9f1e9cda', extraction).
narrative_ontology:cs_interpretation_layer_present('095b7ea1-4284-40b4-92c2-ddbd9f1e9cda').
narrative_ontology:cs_reading_relation('095b7ea1-4284-40b4-92c2-ddbd9f1e9cda', equality_clause_scope__restrictive_originalist, forecloses).
narrative_ontology:cs_reading_relation('095b7ea1-4284-40b4-92c2-ddbd9f1e9cda', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('095b7ea1-4284-40b4-92c2-ddbd9f1e9cda', foundational, equality_universally_self_evident).
narrative_ontology:cs_axiom_status(equality_universally_self_evident, holdable).
narrative_ontology:cs_axiom_grounding('095b7ea1-4284-40b4-92c2-ddbd9f1e9cda', equality_universally_self_evident, deontological).
narrative_ontology:cs_axiom('095b7ea1-4284-40b4-92c2-ddbd9f1e9cda', foundational, historical_exclusions_illegitimate_hypocrisy).
narrative_ontology:cs_axiom_status(historical_exclusions_illegitimate_hypocrisy, holdable).
narrative_ontology:cs_axiom_grounding('095b7ea1-4284-40b4-92c2-ddbd9f1e9cda', historical_exclusions_illegitimate_hypocrisy, deontological).
narrative_ontology:cs_axiom('095b7ea1-4284-40b4-92c2-ddbd9f1e9cda', secondary, judicial_interpretation_legitimacy_for_scope_expansion).
narrative_ontology:cs_axiom_status(judicial_interpretation_legitimacy_for_scope_expansion, holdable).
narrative_ontology:cs_axiom_grounding('095b7ea1-4284-40b4-92c2-ddbd9f1e9cda', judicial_interpretation_legitimacy_for_scope_expansion, conventional).
narrative_ontology:cs_reference_frame('095b7ea1-4284-40b4-92c2-ddbd9f1e9cda', universal_human_dignity_framework).
narrative_ontology:cs_drift_state('095b7ea1-4284-40b4-92c2-ddbd9f1e9cda', contemporary_jurisprudence_late_20th_century, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('095b7ea1-4284-40b4-92c2-ddbd9f1e9cda', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__expansive_universalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, historically_excluded_groups).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, rights_expansion_advocates).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, traditional_power_holders).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, originalist_constitutional_interpreters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rely on the expansive reading to claim constitutional protection for rights previously denied (voting, property ownership, legal personhood, bodily autonomy, dignitary protections). They are the presumptive beneficiaries of universal-equality interpretation. Their power is organized through civil rights movements, legal advocacy, and democratic coalitions; their exit option is constrained to working within constitutional frames or extraconstitutional resistance. They bear the cost of ongoing exclusion when the reading's protections are not enforced.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, historically_excluded_groups, beneficiary,
    organized, generational, constrained, national).

% Include civil rights organizations, progressive legal scholars, and judicial coalitions that advance the expansive reading through litigation, academic argument, and legislative advocacy. They gain institutional legitimacy and agenda-setting power from the reading's acceptance and expansion. Their arbitrage-grade exit options allow them to shift legal strategy, exit particular jurisdictions, or appeal to higher courts.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, rights_expansion_advocates, beneficiary,
    institutional, generational, arbitrage, national).

% Historically benefited from narrow interpretations that restricted equality's scope to their own class. They bear the cost of the expansive reading through redistribution of legal rights, access to public goods, and status loss. Their constrained exit options include constitutional amendment (difficult and collective), emigration (practically unavailable), or retreat to sub-constitutional private ordering (shrinking as public law expands). Their substantial power is directed against the reading's expansion.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, traditional_power_holders, payer,
    powerful, generational, constrained, national).

% Argue that the Constitution's text, as ratified, constrained equality to a limited 18th-century class. The expansive reading delegitimizes their interpretive method and judicial authority. They are payers because the reading's success reduces their institutional standing; they are observers because they participate in the constitutional contest as analytical voices. Their mobile exit options allow them to publish, mentor, or shift to private practice.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, originalist_constitutional_interpreters, payer,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__expansive_universalist, originalist_constitutional_interpreters, observer).

% Advocate that equality's scope should expand through democratic amendment and ratification, not judicial reinterpretation. They would argue for the progressive_textualist middle position that honors democratic process. They are excluded from the expansive reading's own councils — their voice rejects the reading's legitimacy premise (that judges can declare universal equality without amendment) even while potentially supporting its outcomes. Their constrained exit is to work within amendment processes or accept judicial overreach as accomplished fact.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, democratic_amendment_proponents, excluded,
    institutional, generational, constrained, national).

% Possess the institutional authority to declare what the Constitution means and to enforce those declarations. Under the expansive reading, judges have broad discretion to expand equality's scope via interpretation, justified by the claim that equality is self-evident and cannot be bound by historical exclusions. They are agenda-setters because their decisions establish which reading prevails in binding doctrine. Their exit options are identity-locked — the judicial role is constitutive of their position; to exit is to cease being a judge.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, constitutional_judges, agenda_setter,
    institutional, generational, identity_locked, national).

% Shift over time as the reading takes hold. Initially, traditional majorities resist the expansive reading as threatening inherited positions. Later, successful rights expansions create new coalitions where historically excluded groups exercise political power. The reading's success reshapes political alignments. They are observers because they do not directly control constitutional reading but are reshaped by it.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, political_majority_coalitions, observer,
    powerful, biographical, mobile, national).

% Constitutional scholars, philosophers, and historians who analyze whether the expansive reading is coherent, empirically grounded, or strategically sound. They take no direct stake but provide the intellectual scaffolding for the debate and examine the reading's internal consistency.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, analytical_observers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__expansive_universalist, rights_expansion_advocates).
narrative_ontology:fixing_cost_class(equality_clause_scope__expansive_universalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a normative baseline for legal equality that unifies diverse excluded groups around a shared constitutional claim: if the Constitution's equality principle is self-evidently universal, then historical exclusions are violations, not legitimate constitutional boundaries. This coordinates resistance to inherited status hierarchies under a single interpretive frame.
% TRANSFER_FUNCTION: Moves legal status and associated rights (political voice, property rights, bodily autonomy, dignitary protections) from traditional power holders (18th-century property-owning males and their descendants) to historically excluded groups (women, racial minorities, sexual minorities, economically dispossessed). The transfer is implemented through judicial interpretation that declares prior narrow readings illegitimate and expands constitutional guarantees.
% ABSENT_VOICES: Those who would defend the original narrow scope as coherent rather than hypocritical — originalists and historical essentialists who see the 18th-century ratification as a binding limitation, not as a hypocrisy awaiting correction. Their absence from the expansive reading's own councils means the reading does not seriously engage with the claim that historical exclusions were integral to the constitutional bargain rather than departures from it. Democratic amendment proponents are also marginalized — they would argue for legitimacy through democratic process, not judicial self-evidence.
% DISAPPEARANCE_RATIONALE: If the expansive reading vanished and originalist or restrictive readings took hold exclusively, the legal architecture of rights would collapse to historical baselines: voting would be restricted, property discrimination would be constitutionally permitted, gender and racial hierarchies would have no constitutional check, and bodily autonomy claims would lack constitutional footing. The world rearranges because the reading has become institutionalized in federal jurisprudence, state constitutions, and civil rights statutes; removing it would require dismantling that entire structure.
% FOUNDING_PROBLEM: Historical exclusion from the Constitution's promise — women, enslaved and formerly enslaved people, non-property-owning persons, and various minorities were denied the rights the Constitution ostensibly guaranteed to all. The expansive reading claims to solve this by asserting that the promise was always universal and exclusions were always illegitimate.
% FOUNDING_PROBLEM_CORROBORATION: Historians and civil rights advocates attest that historical exclusions were systematic and not incidental — they confirm the founding problem exists. Originalists and constitutional historians dispute whether exclusions violated the Constitution as written or were faithful to the ratification bargain. The Supreme Court in landmark decisions (14th Amendment interpretation, voting rights, equal protection expansion) has endorsed the expansive reading's core claim (that equality applies universally), but this endorsement comes from within the reading's own judicial constituency, not from independent external corroboration. Scholarly consensus outside law (in history, philosophy, human rights discourse) supports the universal-equality framing, but this is not strictly within the constitutional tradition itself.
narrative_ontology:disappearance_verdict(equality_clause_scope__expansive_universalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__expansive_universalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__expansive_universalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equality_clause_scope__expansive_universalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__expansive_universalist, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__expansive_universalist_tests).
:- end_tests(equality_clause_scope__expansive_universalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The reading is authored as tangled_rope: it accomplishes genuine coordination (unifying diverse excluded groups around a shared constitutional claim of universal equality) AND imposes asymmetric extraction (judges expand the Constitution's scope beyond what originalist readings permit, overriding traditional power holders' inherited legal positions without those power holders' consent). Extractiveness is moderate-low (0.38) because the reading's core beneficiaries (historically excluded groups) are motivated by harm-reduction and dignity claims, not by concentrated rents — there is no single institutional seat capturing gains from the constraint. The reading coordinates a normative claim (universal equality is self-evident) with real institutional power (judicial authority) but does not concentrate the benefits. Suppression is moderate (0.52) because originalist jurisprudence mounts real resistance, constitutional traditionalists argue against the reading's legitimacy, and some jurisdictions resist enforcement. The temporal series show suppression_requirement declining (from 0.65 to 0.52) as the reading becomes more institutionalized and culturally accepted — less active repression is needed once the norm is internalized. Base_extractiveness remains stable after t=30 because the reading has reached its institutionalized equilibrium: it is no longer expanding into new domains, but it is fully embedded in constitutional doctrine. Theater_ratio stays low (0.21) because the reading's enforcement is substantive (real rights protections, real redistribution of legal status) rather than performative; judicial decisions have actual consequences for voting access, property rights, dignitary protections, and institutional inclusion.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (traditional power holders, originalist judges) and beneficiary seats (historically excluded groups, rights advocates) should compute different types from the same structural data. From the traditional power holders' position, the reading is a constraint imposed by judicial overreach, extracting their inherited legal privileges without consent or democratic amendment. From the beneficiary position, the reading is genuine coordination that corrects historical hypocrisy and fulfills the Constitution's true promise. The engine computes both positions from the structural directionality: traditional power holders have high d (targets of expansion) and originalist judges have high d (their methodological authority is delegitimized), while historically excluded groups have low d (beneficiary seats gaining rights and status) and civil rights advocates have low d (institutional beneficiaries). These divergences are structural, not a failure of the classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically excluded groups: low directionality (d ≈ 0.2), net beneficiaries gaining rights, status, and legal protection; their exit options are constrained (they must work within constitutional frames) but their power is organized through social movements. Rights expansion advocates: very low directionality (d ≈ 0.1), institutional beneficiaries whose legitimacy depends on the reading's acceptance and expansion; their exit options are arbitrage-grade (they can shift legal strategy or exit particular battles), and their power is institutional. Traditional power holders: high directionality (d ≈ 0.8), targets of extraction; they lose inherited legal privileges, access to hierarchical ordering, and status immunity. Their exit options are constrained (they cannot exit the national legal order easily), though their power is substantial (they can mount political resistance, slow enforcement, seek constitutional amendment). Originalist constitutional interpreters: high directionality (d ≈ 0.75), targets whose interpretive authority is delegitimized; their exit options are mobile (they can move to private practice, academia, or non-constitutional fields), though their institutional identity is tied to constitutional interpretation. Constitutional judges: very low directionality (d ≈ 0.15), agenda-setters who benefit from broad interpretive discretion and expanded authority; their exit options are identity-locked (the role is constitutive), but they exercise power over all other seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The expansive universalist reading faces a mandatrophy challenge: the founding problem (historical exclusion from the Constitution's promise) was real and grave in the 19th and 20th centuries. However, by the late 20th and 21st centuries, major rights expansions have been achieved through Supreme Court decisions, civil rights legislation, constitutional amendments (13th, 14th, 15th, 19th, 26th), and cultural norm shifts. The question is whether the reading's core function — correcting historical hypocrisy by asserting universal equality — remains live or has become atrophied. The measurement series suggest the reading's mandate is partially dead: suppression_requirement declines significantly from t=0 to t=50 (from 0.65 to 0.52), indicating that the reading faces less active resistance as it becomes culturally institutionalized. However, extractiveness remains stable (0.38) and does not decline, which would indicate a fully resolved mandate. This suggests the reading's function has shifted from corrective (expanding neglected rights) to maintenance (defending achieved rights against reversal). The reading is not fully a piton (which would show high theater_ratio and no beneficiaries) because it still coordinates real harm reduction and dignity claims; historically excluded groups remain organizationally mobilized around the reading. But the measured theater_ratio increase (from 0.08 to 0.21) indicates that an increasing share of the reading's enforcement activity is performative — commemorating achievements, asserting symbolic equality — rather than substantive rights expansion. This is consistent with partial mandatrophy: the reading achieved its original corrective purpose but persists partly through institutional inertia and partly through genuinely live concerns about reversal or incomplete implementation. Originalist counter-movements in contemporary jurisprudence (2020s onward, outside the measured interval) suggest the reading is now facing renewed structural resistance, which would complicate the mandatrophy diagnosis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_evidence_vs_contestation,
    'Is equality truly self-evident (as the expansive reading claims), or does the vehement historical and contemporary dispute over its scope reveal that it is a contestable philosophical claim requiring democratic legitimation, not judicial assertion?',
    'Philosophical and historical analysis of whether self-evidence can coexist with genuine disagreement. Examine whether the reading''s opponents are genuinely failing to see self-evident truth or are engaging in coherent alternative philosophical frameworks. Survey whether cultures that have not inherited the Western equality tradition recognize equality as self-evident or as a historically contingent claim.',
    'If equality is contestable rather than self-evident, the reading''s claim to bypass democratic amendment is delegitimized, and the progressive_textualist reading (requiring amendment for scope expansion) becomes structurally preferable. If the self-evidence claim holds, the expansive reading''s judicial authority is justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(self_evidence_vs_contestation, conceptual, 'Whether the constraint''s core legitimacy premise (equality as self-evident truth) survives philosophical scrutiny given historical contestation.').

omega_variable(
    hypocrisy_vs_legitimate_boundary,
    'Were the historical exclusions from the Constitution''s equality promise best characterized as hypocrisy (the reading''s framing) or as a reflection of genuinely contested foundational premises about human nature, property rights, and political membership that the founding generation took seriously?',
    'Careful historiography examining founding-era debates about who constituted the political community, whether exclusions were deliberate compromises or unexamined assumptions, and whether founding documents themselves contained seeds of universal equality or merely asserted it rhetorically. Examine whether the founders saw themselves as creating universal principle or as governing a specific polity with limited membership.',
    'If exclusions were hypocrisy, the expansive reading''s moral authority is strong, and restoration requires only interpretation, not democratic amendment. If exclusions were legitimate-at-the-time foundations that deserve democratic reconsideration rather than judicial reversal, the progressive_textualist reading''s requirement for amendment becomes more compelling. If the analysis shows intentional exclusion dressed as universal principle, the expansive reading''s characterization is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hypocrisy_vs_legitimate_boundary, empirical, 'Historical question about the founding generation''s intent regarding equality''s scope and whether exclusions were hypocritical or foundational.').

omega_variable(
    suppression_mechanism_internalization,
    'To what extent has the measured suppression (resistance from originalist jurisprudence and constitutional traditionalists) been internalized into the reading''s own constituency, such that many supporters now carry internalized doubt about judicial legitimacy even as they endorse universal equality?',
    'Examine legal scholarship and activist discourse to identify internalized conflict between commitment to universal equality and discomfort with judicial overreach. Track whether successful rights expansions (voting rights, marriage equality) have been followed by stronger calls for constitutional amendment or legislative codification, signaling internalized suppression of judicial authority. Post-exit measurement: if originalist jurisprudence gains institutional seats (Supreme Court majority), observe whether the reading''s supporters pivot toward democratic amendment strategies, indicating the suppression was partly internalized.',
    'If internalized, the measured suppression (0.52) understates the constraint''s effective suppressive force — the targets have partially accepted the framing that judicial expansion lacks democratic legitimacy. This would weaken the reading''s structural durability and make it vulnerable to reversal by originalist judicial coalitions. If suppression is purely external (institutional resistance only), the reading''s foundation is more stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether constitutional traditionalist objections have been internalized by equality advocates themselves, weakening institutional commitment to the reading.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint best understood as a reading of the equality clause (a contestable interpretation of fixed text) or as the articulation of a universal moral principle that happens to be asserted in constitutional documents but is not dependent on their interpretation?',
    'Analyze whether the expansive universalist position claims legitimacy FROM the Constitution (constitutional interpretation) or claims the Constitution''s legitimacy is DERIVED from alignment with universal equality (moral priority). If the former, it is a reading; if the latter, it is a philosophical position that uses constitutional language as rhetorical cover.',
    'If the reading is truly a kernel reading (interpretation of fixed text), then the constraint''s type and extractiveness are determined by the configuration of seats around constitutional authority. If it is a moral position dressed in constitutional language, the reading is actually a different constraint — a political philosophy constraint about universal equality itself, not a constitutional constraint about interpreting the equality clause.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this constraint is a reading of a constitutional text or a moral philosophy using constitutional language.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__expansive_universalist, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equality_clause_scope__expansive_universalist, theater_ratio, 0, 0.08).
narrative_ontology:measurement(equa_tr_t10, equality_clause_scope__expansive_universalist, theater_ratio, 10, 0.12).
narrative_ontology:measurement(equa_tr_t20, equality_clause_scope__expansive_universalist, theater_ratio, 20, 0.16).
narrative_ontology:measurement(equa_tr_t30, equality_clause_scope__expansive_universalist, theater_ratio, 30, 0.2).
narrative_ontology:measurement(equa_tr_t40, equality_clause_scope__expansive_universalist, theater_ratio, 40, 0.21).
narrative_ontology:measurement(equa_tr_t50, equality_clause_scope__expansive_universalist, theater_ratio, 50, 0.21).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equality_clause_scope__expansive_universalist, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(equa_be_t10, equality_clause_scope__expansive_universalist, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(equa_be_t20, equality_clause_scope__expansive_universalist, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(equa_be_t30, equality_clause_scope__expansive_universalist, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(equa_be_t40, equality_clause_scope__expansive_universalist, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(equa_be_t50, equality_clause_scope__expansive_universalist, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equality_clause_scope__expansive_universalist, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(equa_su_t10, equality_clause_scope__expansive_universalist, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(equa_su_t20, equality_clause_scope__expansive_universalist, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(equa_su_t30, equality_clause_scope__expansive_universalist, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(equa_su_t40, equality_clause_scope__expansive_universalist, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(equa_su_t50, equality_clause_scope__expansive_universalist, suppression_requirement, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__expansive_universalist, identity_coordination).
narrative_ontology:boltzmann_floor_override(equality_clause_scope__expansive_universalist, 0.12).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__progressive_textualist).

% DUAL FORMULATION NOTE:
% The equality_clause_scope kernel admits three structurally distinct readings, each instantiating a different constraint with different beneficiary/victim structures, different extraction profiles, and different epistemic legitimacy claims. The expansive_universalist reading (THIS CONSTRAINT) treats equality as self-evident and universal; it coexists with the restrictive_originalist reading (which binds equality to 18th-century ratification) and influences the progressive_textualist reading (which agrees on universal scope but requires democratic amendment rather than judicial interpretation for expansion). All three readings share the same kernel text but produce different constraint classifications and extraction profiles. Decomposition per OQ-258: the readings' ε values differ substantially — expansive_universalist treats historical exclusions as pure extraction (ε ≈ 0.38 for the standing arrangement of narrowly-scoped equality), restrictive_originalist treats the 18th-century boundary as non-extractive (natural law of constitutional limits), and progressive_textualist treats narrowly-scoped equality as extractive but requires amendment-based correction rather than judicial assertion. They are genuinely different constraints, not different measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equality_clause_scope__expansive_universalist, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
