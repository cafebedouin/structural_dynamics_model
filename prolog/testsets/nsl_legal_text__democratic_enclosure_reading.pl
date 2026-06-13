% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__democratic_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__democratic_enclosure_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nsl_legal_text__democratic_enclosure_reading
 *   human_readable: National Security Law as Democratic Space Enclosure (HK 2020–)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The National Security Law imposed on Hong Kong in June 2020 by the
 *   Beijing central authority via Article 73 of the Basic Law (bypassing Hong
 *   Kong's legislative process) criminalized a broad category of political
 *   speech, assembly, and organizing under vague standards of subversion,
 *   secession, and foreign collusion. This story instantiates the
 *   democratic_enclosure_reading: the NSL is a mechanism for permanent
 *   closure of democratic space and criminalization of dissent. The law's
 *   stated purpose (security, anti-terrorism, constitutional restoration)
 *   forms a cover narrative; the actual structural effect is suppression of
 *   the entire civil-society opposition infrastructure that had mobilized in
 *   2019 and transformation of Hong Kong's legal order from Common Law
 *   autonomy to Beijing-aligned security apparatus dominance. The readable
 *   extraction is asymmetric: Beijing and the Hong Kong executive
 *   establishment benefit from consolidated security control and elimination
 *   of electoral liability; civil society, press, opposition parties, and
 *   ordinary citizens bearing dissident thought bear the costs. The metric
 *   profile (high extractiveness 0.89, very high suppression 0.91, rising
 *   theater 0.68) and stakeholder structure (identified beneficiaries,
 *   identified victims, active enforcement) mark this as a snare: the
 *   coordination story (security, constitutional restoration) is a cover;
 *   persistence depends on coercion; alternatives are suppressed; and
 *   identifiable victims exist. This is the democratic_enclosure_reading of
 *   the NSL kernel — one of three structurally distinct readings (the
 *   kernel_context and cs_structure sections model the contest).
 *
 * KEY AGENTS:
 *   - beijing_central_authority — imposer of the framework, ultimate beneficiary, maintains kernel interpretation authority
 *   - hong_kong_executive_establishment — operates enforcement machinery, institutional beneficiary, trapped in Beijing-dependent equilibrium
 *   - civil_society_organizations — NGOs, unions, advocacy groups; primary victims; identity-locked to institutional role
 *   - independent_press — investigative journalism; victim seat; constrained exit (emigration breaks institutional base)
 *   - opposition_political_parties — electoral opposition; victim seat; disqualified/imprisoned; identity-locked
 *   - protest_movements — organized dissent; victim seat; geographic exit only viable
 *   - academic_researchers — empirical study of HK politics; victim seat through institutional pressure and legal jeopardy
 *   - legal_professionals — defense counsel, bar association; victim seat through disbarment threat and retaliation
 *   - ordinary_citizens — benefit nominally from security framing; pay through self-censorship, surveillance, psychological cost
 *   - international_observers — excluded; report NSL as human-rights violation; powerless to contest Beijing's constitutional reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, 0.89).
domain_priors:suppression_score(nsl_legal_text__democratic_enclosure_reading, 0.91).
domain_priors:theater_ratio(nsl_legal_text__democratic_enclosure_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, accessibility_collapse, 0.93).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__democratic_enclosure_reading, snare).
narrative_ontology:human_readable(nsl_legal_text__democratic_enclosure_reading, "National Security Law as Democratic Space Enclosure (HK 2020–)").
narrative_ontology:topic_domain(nsl_legal_text__democratic_enclosure_reading, "constitutional/political").

domain_priors:requires_active_enforcement(nsl_legal_text__democratic_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__democratic_enclosure_reading, '5a06e738-f68d-4aa3-9699-b9128c1969ea').
narrative_ontology:cs_kernel_codification('5a06e738-f68d-4aa3-9699-b9128c1969ea', formalized).
narrative_ontology:cs_authority_grounding('5a06e738-f68d-4aa3-9699-b9128c1969ea', extraction).
narrative_ontology:cs_interpretation_layer_present('5a06e738-f68d-4aa3-9699-b9128c1969ea').
narrative_ontology:cs_reading_relation('5a06e738-f68d-4aa3-9699-b9128c1969ea', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('5a06e738-f68d-4aa3-9699-b9128c1969ea', nsl_legal_text__jurisdictional_capture_reading, coexists_with).
narrative_ontology:cs_axiom('5a06e738-f68d-4aa3-9699-b9128c1969ea', foundational, suppression_of_dissent_is_primary_function).
narrative_ontology:cs_axiom_status(suppression_of_dissent_is_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('5a06e738-f68d-4aa3-9699-b9128c1969ea', suppression_of_dissent_is_primary_function, empirically_contingent).
narrative_ontology:cs_axiom('5a06e738-f68d-4aa3-9699-b9128c1969ea', foundational, democratic_legitimacy_requires_organized_opposition).
narrative_ontology:cs_axiom_status(democratic_legitimacy_requires_organized_opposition, holdable).
narrative_ontology:cs_axiom_grounding('5a06e738-f68d-4aa3-9699-b9128c1969ea', democratic_legitimacy_requires_organized_opposition, deontological).
narrative_ontology:cs_reference_frame('5a06e738-f68d-4aa3-9699-b9128c1969ea', hong_kong_autonomous_legal_order).
narrative_ontology:cs_drift_state('5a06e738-f68d-4aa3-9699-b9128c1969ea', post_nsl_imposition, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('5a06e738-f68d-4aa3-9699-b9128c1969ea', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, beijing_central_authority).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, hong_kong_executive_establishment).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, civil_society_organizations).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, independent_press).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, opposition_political_parties).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, protest_movements).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, academic_researchers).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, legal_professionals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, ordinary_citizens).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, ordinary_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Imposed the NSL framework unilaterally via Article 73 of the Basic Law, bypassing Hong Kong legislative process. Interprets the law expansively to criminalize speech, assembly, and organization deemed separatist, subversive, or destabilizing. Maintains the kernel text as fixed authority while delegating enforcement to Hong Kong institutions captured for the purpose.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, beijing_central_authority, agenda_setter,
    institutional, civilizational, analytical, national).

% Operates the NSL enforcement machinery on the ground: police investigations, prosecutions, media control, civil service loyalty vetting. Benefits from centralized executive power, reduced legislative friction, and elimination of electoral liability. Trapped by institutional dependency on Beijing's security guarantee; exit would mean institutional dissolution.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hong_kong_executive_establishment, beneficiary,
    institutional, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__democratic_enclosure_reading, hong_kong_executive_establishment, agenda_setter).

% NGOs, labor unions, and advocacy groups operate under permanent legal jeopardy: organizing, funding, foreign contact, and public messaging all carry NSL liability. Many have dissolved preemptively or self-censored to functional paralysis. Exit means abandoning institutional identity and leaving vulnerable constituencies without advocacy infrastructure.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, civil_society_organizations, payer,
    powerless, biographical, identity_locked, regional).

% Journalists face prosecution for reporting on protests, police conduct, or CCP criticism — the NSL provides the legal tool; editorial independence is treated as sedition. Major outlets have ceased investigative reporting on sensitive topics or been seized by pro-Beijing ownership. International correspondents depart; emigration is de facto exit but means loss of institutional base and credibility within Hong Kong.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, independent_press, payer,
    moderate, biographical, constrained, regional).

% Disqualified from running candidates, dissolved, or leadership imprisoned under NSL on sedition charges. Political opposition is no longer a viable institutional strategy — the legal framework makes organized political dissent criminal. Members remain identity-locked: continuing as a political entity invites prosecution; dissolution means abandonment of political identity and constituencies.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, opposition_political_parties, payer,
    powerless, biographical, identity_locked, regional).

% Assembly, protest, and organizing carry NSL liability for subversion or foreign collusion. The 2019 protest movement was decapitated; subsequent gatherings face police raids and participant arrests. Geographic exit (emigration) is the only real option, but separates activists from affected communities and breaks organizational continuity.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, protest_movements, payer,
    organized, biographical, trapped, regional).

% Research on Hong Kong politics, public opinion, police conduct, or NSL impacts itself becomes legally risky — student inquiries, survey methodology, findings publication all attract scrutiny. University administrators face institutional pressure to police faculty and curriculum. Researchers emigrate or self-censor; institutional autonomy is subordinated to state security concerns.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, academic_researchers, payer,
    moderate, biographical, constrained, regional).

% Defense lawyers defending NSL cases face disbarment threats, prosecution, and political retaliation. Bar associations are pressured to discipline members who take politically sensitive cases. The profession's independence is subordinated to the security framework; many attorneys emigrate or retire from contentious practice.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, legal_professionals, payer,
    moderate, biographical, constrained, regional).

% UN bodies, foreign governments, and human-rights organizations document NSL as democratic enclosure and human-rights violation. Their reports are ignored or dismissed as foreign interference. They are excluded from the legitimacy-grounding framework and have no standing to contest Beijing's constitutional interpretation.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, international_observers, excluded,
    powerful, biographical, analytical, universal).

% Nominally benefit from public order and anti-terrorism framing; in practice face pervasive self-censorship, surveillance, and pressure to report on neighbors/colleagues. Political speech becomes a private act; public discourse narrows. The diffuse security benefit is incommensurate with the psychological and political cost of living under permanent legal jeopardy for thought and speech.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, ordinary_citizens, beneficiary,
    powerless, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__democratic_enclosure_reading, ordinary_citizens, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__democratic_enclosure_reading, beijing_central_authority).
narrative_ontology:fixing_cost_class(nsl_legal_text__democratic_enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stated coordination problem is preventing terrorism, separatism, and foreign interference in Hong Kong post-2019; the law is presented as restoring constitutional order and public security after sustained civil unrest and violence.
% TRANSFER_FUNCTION: Transfers political agency, democratic space, and legal autonomy from Hong Kong civil society and opposition to the Beijing-aligned executive establishment. The mechanism moves legal liability and personal risk from the state onto citizens for exercise of speech, assembly, and organization.
% ABSENT_VOICES: The Hong Kong population as a democratic constituent — citizens are excluded from the constitutional amendment process (Article 73 imposes the law without legislative debate), from ratification, and from legal challenge. Domestic opposition parties are either disqualified or imprisoned before they can mount organized resistance. International observers and foreign governments are dismissed as foreign interference.
% DISAPPEARANCE_RATIONALE: If the NSL framework vanished, Hong Kong's electoral institutions (though already controlled) would functionally revive; opposition parties would reorganize; civil society would resume public advocacy; the independent press would resume investigative reporting; and the political opposition would have legal standing to contest government policy. The democratic political economy of Hong Kong would reorganize around competitive elections and civil society advocacy rather than security-apparatus dominance.
% FOUNDING_PROBLEM: The 2019 anti-extradition bill protests escalated into sustained civil unrest, vandalism, violence, and paralyzing strikes. Beijing interpreted the unrest as separatist-led foreign-backed destabilization of constitutional order and saw the Common Law legal system as unable or unwilling to contain it.
% FOUNDING_PROBLEM_CORROBORATION: Beijing and the Hong Kong executive attest the founding problem is live and active: foreign support for Hong Kong independence remains a security threat. Civil society, international observers, and independent researchers attest the founding problem was the government's legitimacy crisis (triggered by withdrawal of the extradition bill but rooted in democratic deficit and police conduct); the NSL does not solve the problem but eliminates the mechanism (protest, opposition, journalism) for its democratic resolution. No external corroborating authority outside Beijing/HK establishment circles supports the continuing-threat reading.
narrative_ontology:disappearance_verdict(nsl_legal_text__democratic_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__democratic_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__democratic_enclosure_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(nsl_legal_text__democratic_enclosure_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__democratic_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__democratic_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.89) because the NSL transfers political agency and legal autonomy from Hong Kong civil society to Beijing/HK executive without compensation or reciprocal benefit to victims. The law's commission charges are fully borne by payer seats (civil society, press, opposition); beneficiary seats (Beijing, executive) collect political control and elimination of electoral friction. Suppression is even higher (0.91) because the constraint's persistence depends entirely on active enforcement: legal threat, prosecution, police raids, institutional pressure. Without enforcement, civil society would immediately resume organizing; opposition would contest elections; press would resume investigation. There is no internalized norm or coordination equilibrium — the suppression is structural and must be continuously applied. Theater_ratio is moderate-high (0.68) and rising because while genuine security arrests occur (espionage, violence), the majority of enforcement actions target political speech, organizing, and journalism. The ratio grows as enforcement machinery shifts toward pure political suppression after the founding security problem (2019 unrest) resolves. Accessibility_collapse is very high (0.93) because all meaningful alternatives to NSL-governed life have closed: emigration is the only real exit, but costs personal/institutional identity; staying means accepting the constraint's terms. Resistance at 0.72 is substantial but declining (0.76 org level in 2020 → 0.52 in 2026) because the constraint decapitates organized resistance leadership (imprisonment, disqualification) faster than it regenerates. All metrics are measured on a shared six-year time grid (2020 interval start at NSL imposition, 2026 interval end at current projected state).
 *
 * PERSPECTIVAL GAP:
 *   The Beijing authority and Hong Kong executive seats should compute as beneficiaries with low d (near 0.0–0.2 range): they collect political control, face no legal jeopardy, control rule interpretation, and have institutional arbitrage (relationship to Chinese state offers protection). The civil society, press, opposition, and researcher seats should compute as targets with high d (near 0.8–1.0 range): they bear legal jeopardy, constrained/identity-locked exit, suppression enforcement, and institutional vulnerability. Ordinary citizens sit ambiguously: they nominally benefit from security framing (low d) but pay through self-censorship and surveillance (higher d). The engine computes this divergence from stakeholder roles and exit options; the divergence itself is the perspectival gap that explains why the same constraint is read as security restoration by beneficiary seats and democratic closure by victim seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beijing_central_authority and hong_kong_executive_establishment are structural beneficiaries (institutional power, control of law interpretation, collection of political control rent, no exit pressure). Directionality for these seats approaches 0.0–0.15 (subsidy/benefit). Civil_society_organizations, independent_press, opposition_political_parties, and protest_movements are structural targets (constrained/identity_locked exit, legal jeopardy, suppression enforcement targeting them specifically). Directionality for these seats approaches 0.85–1.0 (full target). Academic_researchers and legal_professionals are moderate targets (constrained exit, institutional pressure, but some professional arbitrage available). Ordinary_citizens are complex: they nominally benefit from security framing (organized resistance is suppressed) but pay through psychological suppression and self-censorship. The overall constraint extracts from powerless and moderate seats (civil society, researchers, ordinary citizens) and concentrated beneficiaries (institutional establishment). No directionality override is necessary; the structural derivation from role + exit + power produces the correct d profile.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (2019 unrest, organized opposition, paralyzing protests, violence) was functionally resolved by 2023-24: organized protest movements were decapitated, opposition parties disqualified or imprisoned, strike capacity eliminated, international coordination severed. Yet the NSL intensifies: theater_ratio rises from 0.45 to 0.68, suppression_requirement rises from 0.82 to 0.91, base_extractiveness rises from 0.76 to 0.89. This is the mandatrophy pattern: the founding justification has died, but the constraint persists and extracts more. The theater_ratio rise is the diagnostic signal: if security were the genuine concern, enforcement would stabilize or decline after the threat ends. Instead, enforcement shifts toward political suppression (opposition journalism, academic research, organizing) — the constraint's primary function is now pure political extraction, justified by institutional inertia and the narrative that the founding security threat remains live (contested per founding_problem_status). The constraint has crossed into purely extractive operation while wearing the security coordination costume. This is why mandatrophy analysis classifies it as snare despite the state security framing: the state has captured the constraint, eliminated alternatives, and now extracts indefinitely. The mismatch between founding_problem_status='contested' and disappearance_verdict='world_rearranges' is the mandatrophy marker: the world would rearrange (resumption of opposition organizing, press independence, civil society resurgence) if the constraint vanished, proving alternatives exist and were suppressed, not naturally eliminated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_internalization_trajectory,
    'Is the measured suppression (0.91) primarily structural (legal jeopardy, enforcement machinery, career risk) or internalized (citizens have adopted the constraint''s security narrative, believe dissent is dangerous, self-police thought)?',
    'Post-constraint relaxation data: if suppression persists after NSL enforcement is suspended or terms are materially narrowed, a substantial internalization component is present. Psychological assessment of affected populations would distinguish structural vs. internalized fear.',
    'If substantially internalized, the constraint''s effective suppression is higher than structural measures suggest — the constraint travels with individuals beyond formal legal jeopardy, requiring deeper deprogramming to restore democratic speech capacity. Conversely, if primarily structural, relaxing enforcement pressure and legal threat would rapidly restore dissent and organizing capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_trajectory, empirical, 'Structural vs. internalized suppression mechanism in NSL enforcement').

omega_variable(
    theater_ratio_escalation_mechanism,
    'Does the rising theater_ratio (0.45→0.68) represent genuine shift in enforcement priorities (moving from security-functional arrests to political theater), or consistent theater throughout with the metric merely reflecting growing awareness of the political function?',
    'Detailed arrest-data analysis: proportion of NSL arrests on security grounds (explosives, espionage, genuine terrorism) vs. political organizing grounds. If the functional ratio is stable over time but political salience rises, theater is constant; if arrest categories shift toward political charges, theater is genuinely increasing.',
    'If theater is increasing, the constraint is transitioning from coordination-with-extraction (justified partly on real security) to pure political suppression (justification is increasingly thin). This would push the type toward pure snare. If theater is constant, the constraint was always primarily political; the rising metric reflects only visibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_escalation_mechanism, empirical, 'Whether theater_ratio increase is genuine functional shift or heightened awareness').

omega_variable(
    kernel_reading_contest_structure,
    'This constraint instantiates the democratic_enclosure_reading of the NSL kernel. The sibling sovereignty_restoration_reading describes the same legal text as legitimate restoration of constitutional order post-unrest. Can both readings be structurally correct, or does one reading''s core premise logically foreclose the other?',
    'Examine the foundational claim of each reading: democratic_enclosure asserts suppression of dissent as the primary effect; sovereignty_restoration asserts restoring legitimate constitutional authority as the primary effect. If suppression and constitutional restoration are functionally equivalent (i.e., constitutional restoration entails suppression of what was unconstitutional dissent), the readings coexist. If suppression is a side effect of restoration, restoration_reading would foreclose enclosure_reading. If enclosure is the primary effect and restoration a cover story, enclosure_reading forecloses restoration.',
    'The relation determines how the corpus models the kernel contest. If forecloses, one reading is empirically true and the other is ideologically motivated misrepresentation. If coexists_with, both readings remain live positions held by different parties. If influences, one reading creates structural pressure on the other without eliminating it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Logical structure of the kernel reading contest').

omega_variable(
    mandatrophy_founding_problem_mismatch,
    'The founding_problem (2019 unrest) had a definite historical moment and, by 2023-24, had been functionally eliminated (no major protests, no organized opposition). The constraint (NSL) persists and intensifies in extraction. Does this constitute mandatrophy (the founding problem died but the constraint lives)?',
    'Timeline analysis: when did the founding problem (sustained unrest, organized challenge) cease? What is the temporal relationship between that cessation and NSL intensification (theater_ratio rise, new enforcement actions)? If intensification post-dates problem resolution, mandatrophy is evident.',
    'Mandatrophy would indicate the constraint has shifted from coordination-with-legitimate-security-function to pure extraction wearing coordination clothing. The type might compute as snare regardless of any residual security framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_founding_problem_mismatch, empirical, 'Founding problem obsolescence vs. constraint persistence (mandatrophy signal)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__democratic_enclosure_reading, 2020, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t2020, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2020, 0.45).
narrative_ontology:measurement_basis(nsl__tr_t2020, observed).
narrative_ontology:measurement(nsl__tr_t2021, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2021, 0.52).
narrative_ontology:measurement_basis(nsl__tr_t2021, observed).
narrative_ontology:measurement(nsl__tr_t2022, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2022, 0.58).
narrative_ontology:measurement_basis(nsl__tr_t2022, observed).
narrative_ontology:measurement(nsl__tr_t2024, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2024, 0.64).
narrative_ontology:measurement_basis(nsl__tr_t2024, observed).
narrative_ontology:measurement(nsl__tr_t2026, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2026, 0.68).
narrative_ontology:measurement_basis(nsl__tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(nsl__be_t2020, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2020, 0.76).
narrative_ontology:measurement_basis(nsl__be_t2020, observed).
narrative_ontology:measurement(nsl__be_t2021, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2021, 0.81).
narrative_ontology:measurement_basis(nsl__be_t2021, observed).
narrative_ontology:measurement(nsl__be_t2022, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2022, 0.85).
narrative_ontology:measurement_basis(nsl__be_t2022, observed).
narrative_ontology:measurement(nsl__be_t2024, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2024, 0.88).
narrative_ontology:measurement_basis(nsl__be_t2024, observed).
narrative_ontology:measurement(nsl__be_t2026, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2026, 0.89).
narrative_ontology:measurement_basis(nsl__be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t2020, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2020, 0.82).
narrative_ontology:measurement_basis(nsl__su_t2020, observed).
narrative_ontology:measurement(nsl__su_t2021, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2021, 0.85).
narrative_ontology:measurement_basis(nsl__su_t2021, observed).
narrative_ontology:measurement(nsl__su_t2022, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2022, 0.88).
narrative_ontology:measurement_basis(nsl__su_t2022, observed).
narrative_ontology:measurement(nsl__su_t2024, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2024, 0.9).
narrative_ontology:measurement_basis(nsl__su_t2024, observed).
narrative_ontology:measurement(nsl__su_t2026, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2026, 0.91).
narrative_ontology:measurement_basis(nsl__su_t2026, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=2020, tn=2026
narrative_ontology:measurement(nsl__grid_01, nsl_legal_text__democratic_enclosure_reading, accessibility_collapse(class), 2020, 0.75).
narrative_ontology:measurement(nsl__grid_02, nsl_legal_text__democratic_enclosure_reading, accessibility_collapse(class), 2026, 0.92).
narrative_ontology:measurement(nsl__grid_03, nsl_legal_text__democratic_enclosure_reading, accessibility_collapse(individual), 2020, 0.71).
narrative_ontology:measurement(nsl__grid_04, nsl_legal_text__democratic_enclosure_reading, accessibility_collapse(individual), 2026, 0.94).
narrative_ontology:measurement(nsl__grid_05, nsl_legal_text__democratic_enclosure_reading, accessibility_collapse(organizational), 2020, 0.82).
narrative_ontology:measurement(nsl__grid_06, nsl_legal_text__democratic_enclosure_reading, accessibility_collapse(organizational), 2026, 0.96).
narrative_ontology:measurement(nsl__grid_07, nsl_legal_text__democratic_enclosure_reading, accessibility_collapse(structural), 2020, 0.88).
narrative_ontology:measurement(nsl__grid_08, nsl_legal_text__democratic_enclosure_reading, accessibility_collapse(structural), 2026, 0.98).
narrative_ontology:measurement(nsl__grid_09, nsl_legal_text__democratic_enclosure_reading, resistance(class), 2020, 0.69).
narrative_ontology:measurement(nsl__grid_10, nsl_legal_text__democratic_enclosure_reading, resistance(class), 2026, 0.48).
narrative_ontology:measurement(nsl__grid_11, nsl_legal_text__democratic_enclosure_reading, resistance(individual), 2020, 0.58).
narrative_ontology:measurement(nsl__grid_12, nsl_legal_text__democratic_enclosure_reading, resistance(individual), 2026, 0.41).
narrative_ontology:measurement(nsl__grid_13, nsl_legal_text__democratic_enclosure_reading, resistance(organizational), 2020, 0.76).
narrative_ontology:measurement(nsl__grid_14, nsl_legal_text__democratic_enclosure_reading, resistance(organizational), 2026, 0.52).
narrative_ontology:measurement(nsl__grid_15, nsl_legal_text__democratic_enclosure_reading, resistance(structural), 2020, 0.45).
narrative_ontology:measurement(nsl__grid_16, nsl_legal_text__democratic_enclosure_reading, resistance(structural), 2026, 0.31).
narrative_ontology:measurement(nsl__grid_17, nsl_legal_text__democratic_enclosure_reading, stakes_inflation(class), 2020, 0.72).
narrative_ontology:measurement(nsl__grid_18, nsl_legal_text__democratic_enclosure_reading, stakes_inflation(class), 2026, 0.85).
narrative_ontology:measurement(nsl__grid_19, nsl_legal_text__democratic_enclosure_reading, stakes_inflation(individual), 2020, 0.65).
narrative_ontology:measurement(nsl__grid_20, nsl_legal_text__democratic_enclosure_reading, stakes_inflation(individual), 2026, 0.88).
narrative_ontology:measurement(nsl__grid_21, nsl_legal_text__democratic_enclosure_reading, stakes_inflation(organizational), 2020, 0.78).
narrative_ontology:measurement(nsl__grid_22, nsl_legal_text__democratic_enclosure_reading, stakes_inflation(organizational), 2026, 0.93).
narrative_ontology:measurement(nsl__grid_23, nsl_legal_text__democratic_enclosure_reading, stakes_inflation(structural), 2020, 0.85).
narrative_ontology:measurement(nsl__grid_24, nsl_legal_text__democratic_enclosure_reading, stakes_inflation(structural), 2026, 0.91).
narrative_ontology:measurement(nsl__grid_25, nsl_legal_text__democratic_enclosure_reading, suppression(class), 2020, 0.79).
narrative_ontology:measurement(nsl__grid_26, nsl_legal_text__democratic_enclosure_reading, suppression(class), 2026, 0.91).
narrative_ontology:measurement(nsl__grid_27, nsl_legal_text__democratic_enclosure_reading, suppression(individual), 2020, 0.74).
narrative_ontology:measurement(nsl__grid_28, nsl_legal_text__democratic_enclosure_reading, suppression(individual), 2026, 0.89).
narrative_ontology:measurement(nsl__grid_29, nsl_legal_text__democratic_enclosure_reading, suppression(organizational), 2020, 0.85).
narrative_ontology:measurement(nsl__grid_30, nsl_legal_text__democratic_enclosure_reading, suppression(organizational), 2026, 0.94).
narrative_ontology:measurement(nsl__grid_31, nsl_legal_text__democratic_enclosure_reading, suppression(structural), 2020, 0.88).
narrative_ontology:measurement(nsl__grid_32, nsl_legal_text__democratic_enclosure_reading, suppression(structural), 2026, 0.96).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__democratic_enclosure_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nsl_legal_text__democratic_enclosure_reading, 0.12).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__jurisdictional_capture_reading).

% DUAL FORMULATION NOTE:
% The NSL_legal_text kernel decomposes into three structurally distinct readings because the imposed legal text is subject to contested interpretation: what constitutional commitment it instantiates (security authority, democratic autonomy, jurisdictional integration) determines ε, beneficiary/victim structure, and type. This story (democratic_enclosure_reading) reads the NSL as snare suppressing democratic dissent; sibling stories read it as security restoration (mountain/rope candidate) or jurisdictional capture (tangled_rope/snare candidate). All three readings are live; none has been empirically falsified. The family links model the constraint contest: the readings coexist as different parties' structural interpretations of the same kernel text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
