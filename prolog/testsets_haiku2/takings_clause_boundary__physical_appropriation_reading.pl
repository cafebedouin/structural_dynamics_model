% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__physical_appropriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__physical_appropriation_reading, []).

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
 *   constraint_id: takings_clause_boundary__physical_appropriation_reading
 *   human_readable: Fifth Amendment Takings Clause: Physical Appropriation Reading
 *   domain: constitutional_law/property_rights
 *
 * SUMMARY:
 *   The Fifth Amendment's Takings Clause protects against uncompensated
 *   governmental seizure of private property. This reading narrows the
 *   clause's scope to direct physical appropriation—explicit condemnation,
 *   permanent occupation, or physical seizure—while leaving regulatory
 *   restrictions on property use (zoning, environmental protection, land-use
 *   regulation) outside the compensation obligation. Under this reading, a
 *   regulation that eliminates 80% of a property's value is not a taking;
 *   only if the government physically occupies or seizes the land does
 *   compensation apply. The reading instantiates one pole of a fundamental
 *   constitutional contest: whether the Takings Clause protects only against
 *   dispossession or extends to regulation that substantially diminishes
 *   economic value. This story generates the physical-appropriation reading;
 *   sibling constraint stories generate the regulatory-takings and
 *   categorical-per-se readings from the same kernel.
 *
 * KEY AGENTS:
 *   - Legislative government and regulatory agencies: set and enforce the constraint; benefit from broad regulatory authority unencumbered by takings liability.
 *   - Property owners facing regulation: pay the cost of regulatory restrictions through diminished property value without compensation.
 *   - Dispossessed property owners (direct physical seizure): recognized as takings victims entitled to just compensation.
 *   - Environmental and public-interest advocates: benefit from robust regulatory power to protect public goods.
 *   - Takings-doctrine originalists and federal courts: interpret and enforce the boundary between compensable physical appropriation and non-compensable regulation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, 0.68).
domain_priors:suppression_score(takings_clause_boundary__physical_appropriation_reading, 0.72).
domain_priors:theater_ratio(takings_clause_boundary__physical_appropriation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__physical_appropriation_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__physical_appropriation_reading, "Fifth Amendment Takings Clause: Physical Appropriation Reading").
narrative_ontology:topic_domain(takings_clause_boundary__physical_appropriation_reading, "constitutional_law/property_rights").

domain_priors:requires_active_enforcement(takings_clause_boundary__physical_appropriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__physical_appropriation_reading, '9686466b-21e9-41d2-8a4f-268d3d796268').
narrative_ontology:cs_kernel_codification('9686466b-21e9-41d2-8a4f-268d3d796268', fixed_text).
narrative_ontology:cs_authority_grounding('9686466b-21e9-41d2-8a4f-268d3d796268', lineage).
narrative_ontology:cs_interpretation_layer_present('9686466b-21e9-41d2-8a4f-268d3d796268').
narrative_ontology:cs_reading_relation('9686466b-21e9-41d2-8a4f-268d3d796268', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_reading_relation('9686466b-21e9-41d2-8a4f-268d3d796268', takings_clause_boundary__categorical_takings_reading, influences).
narrative_ontology:cs_axiom('9686466b-21e9-41d2-8a4f-268d3d796268', foundational, takings_clause_protects_physical_possession_only).
narrative_ontology:cs_axiom_status(takings_clause_protects_physical_possession_only, holdable).
narrative_ontology:cs_axiom_grounding('9686466b-21e9-41d2-8a4f-268d3d796268', takings_clause_protects_physical_possession_only, empirically_contingent).
narrative_ontology:cs_axiom('9686466b-21e9-41d2-8a4f-268d3d796268', secondary, regulatory_authority_persists_without_compensation_obligation).
narrative_ontology:cs_axiom_status(regulatory_authority_persists_without_compensation_obligation, holdable).
narrative_ontology:cs_axiom_grounding('9686466b-21e9-41d2-8a4f-268d3d796268', regulatory_authority_persists_without_compensation_obligation, deontological).
narrative_ontology:cs_reference_frame('9686466b-21e9-41d2-8a4f-268d3d796268', physical_appropriation_takings_doctrine).
narrative_ontology:cs_drift_state('9686466b-21e9-41d2-8a4f-268d3d796268', contemporary_environmental_regulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9686466b-21e9-41d2-8a4f-268d3d796268', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, legislative_government).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, regulatory_agencies).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, property_owners_facing_regulation).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, dispossessed_property_owners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, environmental_and_public_interest_groups).
narrative_ontology:constraint_vindicates(takings_clause_boundary__physical_appropriation_reading, takings_clause_compensates_physical_not_regulatory).
narrative_ontology:constraint_vindicates(takings_clause_boundary__physical_appropriation_reading, regulatory_power_survives_without_compensation_duty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts legislation and establishes regulatory frameworks that burden property interests. Under this reading, retains broad authority to regulate land use, environmental compliance, zoning, and public safety without compensating property owners for diminished value UNLESS the government physically seizes or permanently occupies the land. Sets the boundary that separates compensable takings from background regulatory risk.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, legislative_government, agenda_setter,
    institutional, generational, analytical, national).

% Enforces environmental, zoning, health, and safety regulations that restrict property use without triggering compensation obligations under this reading. Operates with substantial freedom to regulate provided no direct physical appropriation occurs. Benefits from the bright-line rule: no need to justify diminished-value assessments or pay compensation for regulatory burdens.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__physical_appropriation_reading, regulatory_agencies, beneficiary).

% Bear the economic cost of regulations that diminish property value without compensation. A wetland restriction, endangered species protection, or zoning change may reduce land value by 40–90%, but this reading treats such losses as background regulatory risk rather than compensable takings. Their exit options are constrained: comply with regulation, challenge it in court (often losing), or absorb the loss.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, property_owners_facing_regulation, payer,
    moderate, biographical, constrained, national).

% Face direct physical seizure or permanent occupation—explicit condemnation, military easement, or government installation of infrastructure. This reading recognizes them as takings victims entitled to just compensation. Their position is trapping because the physical appropriation is categorical and the constitutional remedy is recognized, but the compensation process is often protracted and contested.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, dispossessed_property_owners, payer,
    moderate, biographical, trapped, national).

% Benefit from robust regulatory authority to protect wetlands, endangered species, air and water quality, and public lands without paying compensation to affected property owners. This reading's narrow victim set (physical seizure only) enables expansive environmental and public-health regulation. They advocate for this reading to keep regulatory power unencumbered by takings liability.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, environmental_and_public_interest_groups, beneficiary,
    organized, generational, mobile, national).

% Interpret the Fifth Amendment as compensation-triggered only by direct physical appropriation, reading the text's protection of 'property' as addressing historic trespass and eminent domain, not regulation. They see regulatory takings doctrine as a doctrinal innovation that overextends the text's original meaning. They work through constitutional interpretation and judicial appointment.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, takings_doctrine_originalists, observer,
    analytical, generational, analytical, national).

% Would argue for regulatory takings doctrine and categorical takings per se rules, treating diminished value from regulation as compensable under a broader reading of the Takings Clause. They are excluded from the agenda-setting process that fixes this particular reading; their preferred reading remains one of the contested siblings.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, property_rights_advocates, excluded,
    organized, generational, mobile, national).

% Apply this reading in takings litigation, determining whether a challenged government action rises to the level of physical appropriation that triggers compensation. Courts enforce the boundary, hearing evidence on whether an action constitutes a taking under this narrow framing. Their interpretation of what counts as 'permanent occupation' shapes the constraint's operation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__physical_appropriation_reading, legislative_government).
narrative_ontology:fixing_cost_class(takings_clause_boundary__physical_appropriation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reconciles government's regulatory authority to protect public interests (environment, zoning, safety) with the Fifth Amendment's protection against uncompensated takings, by establishing a bright-line rule: compensation is required only for direct physical appropriation, leaving regulation free from takings liability.
% TRANSFER_FUNCTION: Transfers the burden of regulatory losses (diminished property value from zoning, environmental protection, land-use restriction) from government budget to individual property owners, without compensation. Only direct physical seizure triggers a reverse transfer (government must pay just compensation to dispossessed owner).
% ABSENT_VOICES: Property owners facing severe regulatory diminution (wetland protection reducing land value 80%) would argue for regulatory takings doctrine and compensation; they have weak representation in judicial takings doctrine. Environmental advocates and public-interest groups are effectively represented by the beneficiary stakeholders. Regulatory-takings scholars are present in law review but absent from the Supreme Court's majority coalition that has endorsed the physical-appropriation reading.
% DISAPPEARANCE_RATIONALE: If this reading vanished and regulatory takings doctrine expanded, government budgets would face massive new compensation liability for environmental and zoning regulations; property rights would be revalued upward; environmental protection would become costlier and slower to implement; the institutional balance between regulatory authority and property protection would shift decisively toward property owners. Property that is worthless under strict environmental protection might be valuable again under a regulatory-takings regime.
% FOUNDING_PROBLEM: The Fifth Amendment prohibits taking private property 'without just compensation' but the text is ambiguous: does 'taking' mean only direct seizure (the historic meaning, addressing trespass and condemnation), or does it include regulations that diminish value without physical appropriation? The constraint emerged as courts distinguished between explicit condemnation (clearly compensable) and regulatory burden (unclear).
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars (Epstein, Barnett, recent Supreme Court opinions) attest the founding problem is answered by original public meaning: the Clause protected against physical appropriation, not regulation. Regulatory takings advocates (Humbach, Echeverria, dissenting opinions in Lucas and Penn Central) attest the founding problem is unresolved: the original meaning is ambiguous and the constitutional purpose (protecting property) extends to severe regulatory diminution. The empirical record shows that environmental regulation routinely eliminates 80%+ of property value (wetlands cases, endangered species cases, Lucas). No external authority has resolved the interpretive contest.
narrative_ontology:disappearance_verdict(takings_clause_boundary__physical_appropriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__physical_appropriation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__physical_appropriation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(takings_clause_boundary__physical_appropriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__physical_appropriation_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__physical_appropriation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__physical_appropriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading generates substantial extractiveness (0.68 at 2026) because regulatory restrictions impose large costs on property owners with no compensation mechanism—the constraint captures regulatory value-taking by government and shifts the burden entirely to property owners. Suppression is high (0.72) because the boundary between compensable and non-compensable takings is enforced by courts and embedded in property law; property owners have constrained exit (complying with regulation, losing value, or pursuing litigation that usually fails). Theater has grown over time (0.08 in 1950 to 0.28 in 2026): as environmental and public-health regulation has intensified, judicial discussion of the takings boundary has grown more elaborate, but the underlying functional question—should severe regulatory diminution be compensable?—remains unresolved. Measurement series show cumulative growth in all three metrics, reflecting the expansion of environmental and land-use regulation without parallel expansion of compensability. All metrics are authored on a shared temporal grid (1950, 1970, 1990, 2005, 2015, 2026), enabling coherent lifecycle analysis.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (government, regulatory agencies) perceive this reading as principled coordination: a clear, judicially administrable rule that enables public-interest regulation without constant compensation demands. The payer seats (property owners facing regulation, especially those facing extreme diminution) perceive the same reading as extraction masquerading as coordination—the government captures environmental, public-health, and other regulatory gains while property owners bear uncompensated losses. The dispossessed (direct physical seizure) see the reading as partially honest: it recognizes physical taking as compensable but ignores the economic equivalence of severe regulation. Environmental beneficiaries perceive the reading as enabling their collective action solutions without budgetary constraint. The reading's structural asymmetry: government benefits from clear rules + expansive regulatory power; property owners bear costs without compensation option; dispossessed have a recognized remedy but the compensation process is protracted.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from beneficiary/victim declarations and exit options. Government and regulatory agencies are beneficiaries (they retain broad regulatory authority without compensation obligation) with high power and arbitrage-grade exit (they choose which regulations to enforce and can adjust them). Property owners facing regulation are victims (they bear diminished value without compensation) with moderate power and constrained exit (they cannot abandon property or avoid regulation's effect; challenging in court is expensive and usually fails). Dispossessed owners are also victims but with trapped exit (physical appropriation is categorical and legally recognized, but the compensation process is slow and contested). Environmental and public-interest groups are beneficiaries (they gain expansive regulatory authority) with organized power and mobile exit (they can shift advocacy focus). The engine derives directionality from this structure: beneficiaries get d near 0.0 (low effective extraction on them), victims get d near 1.0 (high effective extraction on them), dispossessed at the trapping end of victimization. The constraint shows high suppression because the boundary between compensable and non-compensable taking is enforced by courts and requires litigation to challenge; property owners cannot easily exit or reframe the reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy confusion between pure coordination and pure extraction. The founding problem—reconciling regulatory authority with takings protection—is contested: government and environmental advocates say the problem is solved (the bright-line rule works), while property advocates and regulatory-takings scholars say it persists (severe regulation without compensation is the unresolved problem). The theater ratio has grown (from 0.08 to 0.28) as the constraint's operation has become more elaborate: courts issue longer opinions on what counts as permanent occupation, but the core question (should regulation compensate?) remains unanswered. The theater growth is diagnostic: the constraint is performing coordination (bright-line clarity) but increasingly visibly failing to address the underlying tension (whether regulatory value-taking should be compensable). If theater rises further and the underlying problem remains contested, the constraint approaches piton status (atrophied function maintained through performance). For now, it remains a tangled rope: real coordination function (clear rules, administrable boundary) + asymmetric extraction (government gains, property owners pay). The mandatrophy analysis concludes: this is not a false natural law (the boundary is humanly constructed), not pure extraction (coordination function is genuine), and not pure coordination (extraction is substantial). It is genuinely tangled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    permanent_occupation_boundary,
    'What degree of government intrusion or occupancy constitutes ''permanent occupation'' triggering compensation under this reading? Does temporary easement, recurring use, or conceptual occupation count?',
    'Doctrinal boundary cases and Supreme Court precedent (Loretto, Kaiser Aetna, Pruneyard): examine how courts operationalize the permanent-occupation line. Empirical analysis of case outcomes to detect whether the boundary is stable or drifting.',
    'A narrow, stable boundary strengthens the reading''s bright-line character; a drifting boundary (courts expanding ''occupation'' to include temporary or intermittent government use) suggests the reading is destabilizing toward regulatory takings doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permanent_occupation_boundary, empirical, 'Whether the permanent-occupation threshold remains judicially coherent or erodes through application.').

omega_variable(
    regulatory_takings_doctrinal_pressure,
    'Is the regulatory takings doctrine (Penn Central factors, categorical per se rules in Lucas) a distinct constraint coexisting with this reading, or does it represent an ongoing internal contradiction within takings jurisprudence that will eventually foreclose the physical-appropriation reading?',
    'Constitutional evolution: track Supreme Court composition, major decisions applying takings doctrine, and law review scholarship on whether regulatory takings and physical-appropriation readings are coherent or inherently conflicting. Monitor state constitutions that have adopted regulatory takings (California, Florida, Maine) to see if federal doctrine follows.',
    'If the readings are structurally incompatible, one will foreclose the other through precedent shift; if they coexist indefinitely, they define a fundamental contest in property jurisprudence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_takings_doctrinal_pressure, conceptual, 'Whether regulatory takings doctrine is an alternative reading of the same kernel or a competing constraint that will eventually dominate.').

omega_variable(
    original_public_meaning_vs_evolving_protection,
    'Does the Fifth Amendment''s original public meaning constrain takings doctrine, or does the constraint evolve through application to changed circumstances (industrial regulation, environmental protection, 20th-century property relations)?',
    'Originalist vs. living constitutionalist scholarship; Supreme Court''s methodological choices in takings cases; whether the Court cites original meaning or evolving doctrine when establishing the physical-appropriation boundary.',
    'If original meaning binds, this reading is the correct constitutional constraint; if the Constitution evolves, regulatory takings doctrine has valid claim to the same kernel, and the competition between readings is unresolved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(original_public_meaning_vs_evolving_protection, preference, 'Whether takings doctrine is constrained by historical meaning or by evolving constitutional purposes.').

omega_variable(
    compensability_of_severe_regulatory_burden,
    'For property owners facing 90%+ value diminution from regulation (Lucas-style), does this reading correctly treat the loss as background regulatory risk, or does the severity create an unstated takings obligation that the reading obscures?',
    'Empirical study of property owner experience and remedies in extreme diminution cases; analysis of whether courts recognize an implied takings claim when regulation is catastrophic; legislative responses (buyout programs, hardship relief) that suggest recognition of compensability.',
    'If severe regulatory burdens create hidden compensability, the reading is capturing extraction (government benefit from free value-taking) while misclassifying it as coordination (regulatory authority). If burdens are genuinely treated as background risk, the reading is structurally honest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compensability_of_severe_regulatory_burden, empirical, 'Whether extreme regulatory diminution is actually uncompensated or subject to de facto remedy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__physical_appropriation_reading, 1950, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t1950, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement_basis(taki_tr_t1950, observed).
narrative_ontology:measurement(taki_tr_t1970, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement_basis(taki_tr_t1970, observed).
narrative_ontology:measurement(taki_tr_t1990, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement_basis(taki_tr_t1990, observed).
narrative_ontology:measurement(taki_tr_t2005, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 2005, 0.23).
narrative_ontology:measurement_basis(taki_tr_t2005, observed).
narrative_ontology:measurement(taki_tr_t2015, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 2015, 0.26).
narrative_ontology:measurement_basis(taki_tr_t2015, observed).
narrative_ontology:measurement(taki_tr_t2026, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(taki_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(taki_be_t1950, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1950, 0.45).
narrative_ontology:measurement_basis(taki_be_t1950, observed).
narrative_ontology:measurement(taki_be_t1970, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1970, 0.52).
narrative_ontology:measurement_basis(taki_be_t1970, observed).
narrative_ontology:measurement(taki_be_t1990, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1990, 0.61).
narrative_ontology:measurement_basis(taki_be_t1990, observed).
narrative_ontology:measurement(taki_be_t2005, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement_basis(taki_be_t2005, observed).
narrative_ontology:measurement(taki_be_t2015, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement_basis(taki_be_t2015, observed).
narrative_ontology:measurement(taki_be_t2026, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(taki_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1950, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement_basis(taki_su_t1950, observed).
narrative_ontology:measurement(taki_su_t1970, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1970, 0.62).
narrative_ontology:measurement_basis(taki_su_t1970, observed).
narrative_ontology:measurement(taki_su_t1990, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement_basis(taki_su_t1990, observed).
narrative_ontology:measurement(taki_su_t2005, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement_basis(taki_su_t2005, observed).
narrative_ontology:measurement(taki_su_t2015, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement_basis(taki_su_t2015, observed).
narrative_ontology:measurement(taki_su_t2026, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 2026, 0.72).
narrative_ontology:measurement_basis(taki_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__physical_appropriation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(takings_clause_boundary__physical_appropriation_reading, 0.12).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary__regulatory_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary__categorical_takings_reading).

% DUAL FORMULATION NOTE:
% The takings_clause_boundary kernel admits three structurally distinct constraint readings. (1) This story (physical_appropriation_reading) narrows takings to direct physical appropriation; government retains broad regulatory power without compensation. (2) regulatory_takings_reading extends takings to regulations that diminish value beyond a threshold; expansion of victim set and government compensation obligation. (3) categorical_takings_reading creates per se bright lines for certain categories (permanent occupation, total value elimination); mixed expansion/contraction vs. this reading. All three instantiate the same Fifth Amendment clause; they differ on the boundary between compensable taking and non-compensable regulation. Epsilon values differ substantially: physical-appropriation reading is 0.68 (broad regulatory power preserved), regulatory-takings reading would be ~0.35 (government bears compensation cost), categorical reading is ~0.52 (bright lines reduce litigation but expand compensation obligation). The three stories form a constraint family linked by affects_constraints; each should include network edges to its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(takings_clause_boundary__physical_appropriation_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
